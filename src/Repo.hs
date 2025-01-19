module Repo (
    Repo, Commit, commitId,
    CommitId, textCommitId, showCommitId,
    TreeId, textTreeId, showTreeId,

    openRepo,
    readBranch,
    readTag,
    listCommits,

    getTreeId,
    getCommitTitle,
    getCommitMessage,

    checkoutAt,
    readCommittedFile,

    watchBranch,
    watchTags,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
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

import System.Directory
import System.Exit
import System.FilePath
import System.INotify
import System.Process


data Repo
    = GitRepo
        { gitDir :: FilePath
        , gitLock :: MVar ()
        , gitInotify :: MVar (Maybe ( INotify, TChan Text ))
        , gitWatchedBranches :: MVar (Map Text [ TVar (Maybe Commit) ])
        }

data Commit = Commit
    { commitRepo :: Repo
    , commitId_ :: CommitId
    , commitDetails :: MVar (Maybe CommitDetails)
    }

commitId :: Commit -> CommitId
commitId = commitId_

data CommitDetails = CommitDetails
    { commitTreeId :: TreeId
    , commitTitle :: Text
    , commitMessage :: Text
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

readCommitFromFile :: MonadIO m => Repo -> FilePath -> m (Maybe Commit)
readCommitFromFile commitRepo@GitRepo {..} path = liftIO $ do
    try @IOException (BC.readFile $ gitDir </> path) >>= \case
        Right content | (cid : _) <- BC.lines content -> do
            let commitId_ = CommitId cid
            commitDetails <- newMVar Nothing
            return $ Just Commit {..}
        _ -> do
            return Nothing

readBranch :: MonadIO m => Repo -> Text -> m (Maybe Commit)
readBranch repo branch = readCommitFromFile repo ("refs/heads" </> T.unpack branch)

readTag :: MonadIO m => Repo -> Text -> m (Maybe Commit)
readTag repo tag = readCommitFromFile repo ("refs/tags" </> T.unpack tag)

listCommits :: MonadIO m => Repo -> Text -> m [ Commit ]
listCommits commitRepo range = liftIO $ do
    out <- readProcess "git" [ "log", "--pretty=%H", "--first-parent", "--reverse", T.unpack range ] ""
    forM (lines out) $ \cid -> do
        let commitId_ = CommitId (BC.pack cid)
        commitDetails <- newMVar Nothing
        return Commit {..}


getCommitDetails :: (MonadIO m, MonadFail m) => Commit -> m CommitDetails
getCommitDetails Commit {..} = do
    let GitRepo {..} = commitRepo
    liftIO $ do
        modifyMVar commitDetails $ \case
            cur@(Just details) -> do
                return ( cur, details )
            Nothing -> do
                ( infoPart, _ : title : message ) <-
                    fmap (span (not . null) . lines) $
                    withMVar gitLock $ \_ -> do
                        readProcess "git" [ "--git-dir=" <> gitDir, "cat-file", "commit", showCommitId commitId_ ] ""
                let info = map (fmap (drop 1) . span (/= ' ')) infoPart

                Just commitTreeId <- return $ TreeId . BC.pack <$> lookup "tree" info
                let commitTitle = T.pack title
                let commitMessage = T.pack $ unlines $ dropWhile null message

                let details = CommitDetails {..}
                return ( Just details, details )

getTreeId :: (MonadIO m, MonadFail m) => Commit -> m TreeId
getTreeId = fmap commitTreeId . getCommitDetails

getCommitTitle :: (MonadIO m, MonadFail m) => Commit -> m Text
getCommitTitle = fmap commitTitle . getCommitDetails

getCommitMessage :: (MonadIO m, MonadFail m) => Commit -> m Text
getCommitMessage = fmap commitMessage . getCommitDetails


checkoutAt :: (MonadIO m, MonadFail m) => Commit -> FilePath -> m ()
checkoutAt Commit {..} dest = do
    let GitRepo {..} = commitRepo
    liftIO $ withMVar gitLock $ \_ -> do
        "" <- readProcess "git" [ "clone", "--quiet", "--shared", "--no-checkout", gitDir, dest ] ""
        "" <- readProcess "git" [ "-C", dest, "restore", "--worktree", "--source=" <> showCommitId commitId_, "--", "." ] ""
        removeDirectoryRecursive $ dest </> ".git"


readCommittedFile :: Commit -> FilePath -> IO (Maybe BL.ByteString)
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


repoInotify :: Repo -> IO ( INotify, TChan Text )
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
            let tag = decodeUtf8 $ filePath event
            atomically $ writeTChan tagsChan tag

        return ( Just info, info )
  where
    headsDir = gitDir </> "refs/heads"
    tagsDir = gitDir </> "refs/tags"

watchBranch :: Repo -> Text -> IO (STM (Maybe Commit))
watchBranch repo@GitRepo {..} branch = do
    var <- newTVarIO =<< readBranch repo branch
    void $ repoInotify repo
    modifyMVar_ gitWatchedBranches $ return . M.insertWith (++) branch [ var ]
    return $ readTVar var

watchTags :: Repo -> IO (TChan Text)
watchTags repo = do
    tagsChan <- snd <$> repoInotify repo
    atomically $ dupTChan tagsChan
