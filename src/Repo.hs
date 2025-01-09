module Repo (
    Repo(..), Commit(..),
    CommitId, showCommitId,
    TreeId, showTreeId,

    openRepo,
    listCommits,
    checkoutAt,
    readTreeId,
    readCommittedFile,
) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T

import System.Directory
import System.Exit
import System.FilePath
import System.Process


data Repo
    = GitRepo
        { gitDir :: FilePath
        , gitLock :: MVar ()
        }

data Commit = Commit
    { commitRepo :: Repo
    , commitId :: CommitId
    , commitDescription :: Text
    }


newtype CommitId = CommitId ByteString

showCommitId :: CommitId -> String
showCommitId (CommitId cid) = BC.unpack cid

newtype TreeId = TreeId ByteString

showTreeId :: TreeId -> String
showTreeId (TreeId tid) = BC.unpack tid


openRepo :: FilePath -> IO (Maybe Repo)
openRepo path = do
    findGitDir >>= \case
        Just gitDir -> do
            gitLock <- newMVar ()
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


listCommits :: MonadIO m => Repo -> String -> m [ Commit ]
listCommits commitRepo range = liftIO $ do
    out <- readProcess "git" [ "log", "--pretty=oneline", "--first-parent", "--reverse", range ] ""
    forM (lines out) $ \line -> do
        let ( cid, desc ) = fmap (drop 1) $ (span (/=' ')) line
            commitId = CommitId (BC.pack cid)
            commitDescription = T.pack desc
        return Commit {..}


checkoutAt :: (MonadIO m, MonadFail m) => Commit -> FilePath -> m ()
checkoutAt Commit {..} dest = do
    let GitRepo {..} = commitRepo
    liftIO $ withMVar gitLock $ \_ -> do
        "" <- readProcess "git" [ "clone", "--quiet", "--shared", "--no-checkout", gitDir, dest ] ""
        "" <- readProcess "git" [ "-C", dest, "restore", "--worktree", "--source=" <> showCommitId commitId, "--", "." ] ""
        removeDirectoryRecursive $ dest </> ".git"


readTreeId :: (MonadIO m, MonadFail m) => Commit -> m TreeId
readTreeId Commit {..} = do
    let GitRepo {..} = commitRepo
    liftIO $ withMVar gitLock $ \_ -> do
        [ "tree", tid ] : _ <- map words . lines <$> readProcess "git" [ "--git-dir=" <> gitDir, "cat-file", "commit", showCommitId commitId ] ""
        return $ TreeId $ BC.pack tid


readCommittedFile :: Commit -> FilePath -> IO (Maybe BL.ByteString)
readCommittedFile Commit {..} path = do
    let GitRepo {..} = commitRepo
    liftIO $ withMVar gitLock $ \_ -> do
        let cmd = (proc "git" [ "--git-dir=" <> gitDir, "cat-file", "blob", showCommitId commitId <> ":" <> path ])
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
