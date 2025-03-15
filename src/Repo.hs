module Repo (
    Repo,
    DeclaredRepo(..),
    RepoName(..), textRepoName, showRepoName,
    Commit, commitId,
    CommitId, textCommitId, showCommitId,
    Tree, treeId, treeRepo,
    TreeId, textTreeId, showTreeId,
    Tag(..),

    openRepo,
    readCommit,
    readBranch,
    readTag,
    listCommits,
    findUpstreamRef,

    getCommitTree,
    getCommitTitle,
    getCommitMessage,

    getSubtree,

    checkoutAt,
    createWipCommit,
    readCommittedFile,

    watchBranch,
    watchTags,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (IOException)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Function
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding

import System.Environment
import System.Exit
import System.FilePath
import System.INotify
import System.IO.Temp
import System.Process


data Repo
    = GitRepo
        { gitDir :: FilePath
        , gitLock :: MVar ()
        , gitInotify :: MVar (Maybe ( INotify, TChan (Tag Commit) ))
        , gitWatchedBranches :: MVar (Map Text [ TVar (Maybe Commit) ])
        }

data DeclaredRepo = DeclaredRepo
    { repoName :: RepoName
    , repoPath :: FilePath
    }

newtype RepoName = RepoName Text
    deriving (Eq, Ord)

textRepoName :: RepoName -> Text
textRepoName (RepoName text) = text

showRepoName :: RepoName -> String
showRepoName = T.unpack . textRepoName


data Commit = Commit
    { commitRepo :: Repo
    , commitId_ :: CommitId
    , commitDetails :: MVar (Maybe CommitDetails)
    }

commitId :: Commit -> CommitId
commitId = commitId_

data CommitDetails = CommitDetails
    { commitTree :: Tree
    , commitTitle :: Text
    , commitMessage :: Text
    }

data Tree = Tree
    { treeRepo :: Repo
    , treeId :: TreeId
    }

data Tag a = Tag
    { tagTag :: Text
    , tagObject :: a
    , tagMessage :: Text
    }

instance Eq Repo where
    (==) = (==) `on` gitLock

instance Eq Commit where
    x == y  =  commitRepo x == commitRepo y &&
                commitId_ x == commitId_ y


newtype CommitId = CommitId ByteString
    deriving (Eq, Ord)

textCommitId :: CommitId -> Text
textCommitId (CommitId cid) = decodeUtf8 cid

showCommitId :: CommitId -> String
showCommitId (CommitId cid) = BC.unpack cid

newtype TreeId = TreeId ByteString
    deriving (Eq, Ord)

textTreeId :: TreeId -> Text
textTreeId (TreeId tid) = decodeUtf8 tid

showTreeId :: TreeId -> String
showTreeId (TreeId tid) = BC.unpack tid


runGitCommand :: MonadIO m => Repo -> [ String ] -> m String
runGitCommand GitRepo {..} args = liftIO $ do
    withMVar gitLock $ \_ -> do
        readProcess "git" (("--git-dir=" <> gitDir) : args) ""


openRepo :: FilePath -> IO (Maybe Repo)
openRepo path = do
    findGitDir >>= \case
        Just gitDir -> do
            gitLock <- newMVar ()
            gitInotify <- newMVar Nothing
            gitWatchedBranches <- newMVar M.empty
            return $ Just GitRepo {..}
        Nothing -> do
            return Nothing
  where
    tryGitDir gpath = readProcessWithExitCode "git" [ "rev-parse", "--resolve-git-dir", gpath ] "" >>= \case
        ( ExitSuccess, out, _ ) | dir : _ <- lines out -> return (Just dir)
        _                                              -> return Nothing
    findGitDir = do
        tryGitDir path >>= \case
            Just dir -> return (Just dir)
            Nothing  -> tryGitDir (path </> ".git") >>= \case
                Just dir -> return (Just dir)
                _        -> return Nothing

mkCommit :: MonadIO m => Repo -> CommitId -> m Commit
mkCommit commitRepo commitId_ = do
    commitDetails <- liftIO $ newMVar Nothing
    return $ Commit {..}

readCommit :: (MonadIO m, MonadFail m) => Repo -> Text -> m Commit
readCommit repo@GitRepo {..} ref = liftIO $ do
    readProcessWithExitCode "git" [ "--git-dir=" <> gitDir, "rev-parse", "--verify", "--quiet", T.unpack ref <> "^{commit}" ] "" >>= \case
        ( ExitSuccess, out, _ ) | cid : _ <- lines out -> mkCommit repo (CommitId $ BC.pack cid)
        _                                              -> fail $ "revision `" <> T.unpack ref <> "' not found in `" <> gitDir <> "'"

readCommitFromFile :: MonadIO m => Repo -> FilePath -> m (Maybe Commit)
readCommitFromFile repo@GitRepo {..} path = liftIO $ do
    try @IO @IOException (BC.readFile $ gitDir </> path) >>= \case
        Right content | (cid : _) <- BC.lines content -> do
            Just <$> mkCommit repo (CommitId cid)
        _ -> do
            return Nothing

readBranch :: MonadIO m => Repo -> Text -> m (Maybe Commit)
readBranch repo branch = readCommitFromFile repo ("refs/heads" </> T.unpack branch)

readTag :: MonadIO m => Repo -> Text -> m (Maybe (Tag Commit))
readTag repo tag = do
    ( infoPart, message ) <-
        fmap (fmap (drop 1) . span (not . null) . lines) $
        runGitCommand repo [ "cat-file", "tag", T.unpack tag ]
    let info = map (fmap (drop 1) . span (/= ' ')) infoPart

    sequence $ do
        otype <- lookup "type" info
        guard (otype == "commit")
        tagTag <- T.pack <$> lookup "tag" info
        cid <- CommitId . BC.pack <$> lookup "object" info
        let tagMessage = T.pack $ unlines $ dropWhile null message
        Just $ do
            tagObject <- liftIO $ mkCommit repo cid
            return Tag {..}

listCommits :: MonadIO m => Repo -> Text -> m [ Commit ]
listCommits commitRepo range = liftIO $ do
    out <- runGitCommand commitRepo [ "log", "--pretty=%H", "--first-parent", "--reverse", T.unpack range ]
    forM (lines out) $ \cid -> do
        let commitId_ = CommitId (BC.pack cid)
        commitDetails <- newMVar Nothing
        return Commit {..}

findUpstreamRef :: MonadIO m => Repo -> Text -> m (Maybe Text)
findUpstreamRef repo@GitRepo {..} ref = liftIO $ do
    deref <- readProcessWithExitCode "git" [ "--git-dir=" <> gitDir, "symbolic-ref", "--quiet", T.unpack ref ] "" >>= \case
        ( ExitSuccess, out, _ ) | [ deref ] <- lines out -> return deref
        ( _, _, _ ) -> return $ T.unpack ref
    runGitCommand repo [ "show-ref", deref ] >>= \case
        out | [ _, fullRef ] : _ <- words <$> lines out
            -> runGitCommand repo [ "for-each-ref", "--format=%(upstream)", fullRef ] >>= \case
            out' | [ upstream ] <- lines out'
                -> return $ Just $ T.pack upstream
            _ -> return Nothing
        _ -> return Nothing


getCommitDetails :: (MonadIO m, MonadFail m) => Commit -> m CommitDetails
getCommitDetails Commit {..} = do
    liftIO $ do
        modifyMVar commitDetails $ \case
            cur@(Just details) -> do
                return ( cur, details )
            Nothing -> do
                ( infoPart, _ : title : message ) <-
                    fmap (span (not . null) . lines) $
                    runGitCommand commitRepo [ "cat-file", "commit", showCommitId commitId_ ]
                let info = map (fmap (drop 1) . span (/= ' ')) infoPart

                Just treeId <- return $ TreeId . BC.pack <$> lookup "tree" info
                let treeRepo = commitRepo
                let commitTree = Tree {..}
                let commitTitle = T.pack title
                let commitMessage = T.pack $ unlines $ dropWhile null message

                let details = CommitDetails {..}
                return ( Just details, details )

getCommitTree :: (MonadIO m, MonadFail m) => Commit -> m Tree
getCommitTree = fmap commitTree . getCommitDetails

getCommitTitle :: (MonadIO m, MonadFail m) => Commit -> m Text
getCommitTitle = fmap commitTitle . getCommitDetails

getCommitMessage :: (MonadIO m, MonadFail m) => Commit -> m Text
getCommitMessage = fmap commitMessage . getCommitDetails


getSubtree :: (MonadIO m, MonadFail m) => Maybe Commit -> FilePath -> Tree -> m Tree
getSubtree mbCommit path tree = liftIO $ do
    let GitRepo {..} = treeRepo tree
    readProcessWithExitCode "git" [ "--git-dir=" <> gitDir, "rev-parse", "--verify", "--quiet", showTreeId (treeId tree) <> ":" <> path ] "" >>= \case
        ( ExitSuccess, out, _ ) | tid : _ <- lines out -> do
            return Tree
                { treeRepo = treeRepo tree
                , treeId = TreeId (BC.pack tid)
                }
        _ -> do
            fail $ "subtree `" <> path <> "' not found" <> maybe "" (("in revision `" <>) . (<> "'") . showCommitId . commitId) mbCommit


checkoutAt :: (MonadIO m, MonadFail m) => Tree -> FilePath -> m ()
checkoutAt Tree {..} dest = do
    let GitRepo {..} = treeRepo
    liftIO $ withSystemTempFile "minici-checkout.index" $ \index _ -> do
        curenv <- getEnvironment
        let readGitProcess args input =
                withMVar gitLock $ \_ ->
                    readCreateProcess (proc "git" args)
                        { env = Just $ concat
                            [ [ ( "GIT_INDEX_FILE", index ) ]
                            , [ ( "GIT_DIR", gitDir ) ]
                            , [ ( "GIT_WORK_TREE", "." ) ]
                            , curenv
                            ]
                        } input
        "" <- readGitProcess [ "read-tree", showTreeId treeId ] ""
        "" <- readGitProcess [ "checkout-index", "--all", "--prefix=" <> addTrailingPathSeparator dest ] ""
        return ()

createWipCommit :: (MonadIO m, MonadMask m, MonadFail m) => Repo -> m Commit
createWipCommit repo@GitRepo {..} = do
    withSystemTempFile "minici-wip.index" $ \index _ -> do
        curenv <- liftIO getEnvironment
        let readGitProcess mbWorkTree args input = liftIO $ do
                withMVar gitLock $ \_ ->
                    readCreateProcess (proc "git" args)
                        { env = Just $ concat
                            [ [ ( "GIT_INDEX_FILE", index ) ]
                            , [ ( "GIT_DIR", gitDir ) ]
                            , map (( "GIT_WORK_TREE", ) . T.unpack) $ maybeToList mbWorkTree
                            , curenv
                            ]
                        } input
            mkPair = fmap (T.dropWhile (== ' ')) . T.break (== ' ')
        info <- map mkPair . takeWhile (not . T.null) . T.splitOn "\0". T.pack <$>
            readGitProcess Nothing [ "worktree", "list", "--porcelain", "-z" ] ""
        case ( lookup "worktree" info, lookup "HEAD" info ) of
            ( Just worktree, Just headRev ) -> do
                let readGitProcessW = readGitProcess (Just worktree)

                headCommit <- mkCommit repo (CommitId $ encodeUtf8 headRev)
                headTree <- getCommitTree headCommit

                "" <- readGitProcessW [ "read-tree", "--empty" ] ""
                status <- map mkPair . T.splitOn "\0" . T.pack <$>
                    readGitProcessW [ "status", "--porcelain=v1", "-z", "--untracked-files=all" ] ""
                "" <- readGitProcessW [ "update-index", "--add", "-z", "--stdin" ] $ T.unpack $ T.intercalate "\0" $ map snd status
                [ tid ] <- lines <$> readGitProcessW [ "write-tree" ] ""

                if TreeId (BC.pack tid) == treeId headTree
                  then return headCommit
                  else do
                    headMsg <- getCommitTitle headCommit
                    let wipMsg = case lookup "branch" info of
                            Just branch -> "WIP on " <> branch <> ": " <> headMsg
                            Nothing -> "WIP: " <> headMsg
                    [ cid ] <- lines <$> readGitProcessW [ "commit-tree", "-m", T.unpack wipMsg, "-p", T.unpack headRev, tid ] ""
                    mkCommit repo (CommitId $ BC.pack cid)

            _ -> readCommit repo "HEAD"


readCommittedFile :: MonadIO m => Commit -> FilePath -> m (Maybe BL.ByteString)
readCommittedFile Commit {..} path = do
    let GitRepo {..} = commitRepo
    liftIO $ withMVar gitLock $ \_ -> do
        let cmd = (proc "git" [ "--git-dir=" <> gitDir, "cat-file", "blob", showCommitId commitId_ <> ":" <> path ])
                { std_in = NoStream
                , std_out = CreatePipe
                }
        createProcess cmd >>= \( _, mbstdout, _, ph ) -> if
            | Just stdout <- mbstdout -> do
                content <- BL.hGetContents stdout

                -- check if there will be some output:
                case BL.uncons content of
                    Just (c, _) -> c `seq` return ()
                    Nothing -> return ()

                getProcessExitCode ph >>= \case
                    Just code | code /= ExitSuccess ->
                        return Nothing
                    _ ->
                        return (Just content)
            | otherwise -> error "createProcess must return stdout handle"


repoInotify :: Repo -> IO ( INotify, TChan (Tag Commit) )
repoInotify repo@GitRepo {..} = modifyMVar gitInotify $ \case
    cur@(Just info) ->
        return ( cur, info )
    Nothing -> do
        inotify <- initINotify
        tagsChan <- newBroadcastTChanIO
        let info = ( inotify, tagsChan )

        _ <- addWatch inotify [ MoveIn ] (BC.pack headsDir) $ \event -> do
            let branch = decodeUtf8 $ filePath event
            tvars <- fromMaybe [] . M.lookup branch <$> readMVar gitWatchedBranches
            when (not $ null tvars) $ do
                commit <- readBranch repo branch
                atomically $ do
                    mapM_ (`writeTVar` commit) tvars

        _ <- addWatch inotify [ MoveIn ] (BC.pack tagsDir) $ \event -> do
            readTag repo (decodeUtf8 $ filePath event) >>= \case
                Just tag -> atomically $ writeTChan tagsChan tag
                Nothing  -> return ()

        return ( Just info, info )
  where
    headsDir = gitDir </> "refs/heads"
    tagsDir = gitDir </> "refs/tags"

watchBranch :: MonadIO m => Repo -> Text -> m (STM (Maybe Commit))
watchBranch repo@GitRepo {..} branch = liftIO $ do
    var <- newTVarIO =<< readBranch repo branch
    void $ repoInotify repo
    modifyMVar_ gitWatchedBranches $ return . M.insertWith (++) branch [ var ]
    return $ readTVar var

watchTags :: MonadIO m => Repo -> m (TChan (Tag Commit))
watchTags repo = liftIO $ do
    tagsChan <- snd <$> repoInotify repo
    atomically $ dupTChan tagsChan
