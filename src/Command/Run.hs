module Command.Run (
    RunCommand,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader

import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

import System.Exit
import System.Process

import Command
import Config
import Job
import Repo
import Terminal


data RunCommand = RunCommand Text

instance Command RunCommand where
    commandName _ = "run"
    commandDescription _ = "Execude jobs per minici.yaml for given commits"

    type CommandArguments RunCommand = Maybe Text

    commandUsage _ = T.pack $ unlines $
        [ "Usage: minici run"
        , "         run jobs for commits on current branch not yet in upstream branch"
        , "   or: minici run <ref>"
        , "         run jobs for commits on <ref> not yet in its upstream ref"
        , "   or: minici run <commit>..<commit>"
        , "         run jobs for commits in given range"
        ]

    commandInit _ _ = RunCommand . fromMaybe "HEAD"
    commandExec = cmdRun

cmdRun :: RunCommand -> CommandExec ()
cmdRun (RunCommand changeset) = do
    CommonOptions {..} <- getCommonOptions
    ( base, tip ) <- case T.splitOn (T.pack "..") changeset of
        base : tip : _ -> return ( T.unpack base, T.unpack tip )
        [ param ] -> liftIO $ do
            [ deref ] <- readProcessWithExitCode "git" [ "symbolic-ref", "--quiet", T.unpack param ] "" >>= \case
                ( ExitSuccess, out, _ ) -> return $ lines out
                ( _, _, _ ) -> return [ T.unpack param ]
            [ _, tip ] : _ <- fmap words . lines <$> readProcess "git" [ "show-ref", deref ] ""
            [ base ] <- lines <$> readProcess "git" [ "for-each-ref", "--format=%(upstream)", tip ] ""
            return ( base, tip )
        [] -> error "splitOn should not return empty list"

    tout <- getTerminalOutput
    liftIO $ do
        mngr <- newJobManager "./.minici" optJobs
        Just repo <- openRepo "."
        commits <- listCommits repo (base <> ".." <> tip)
        jobssets <- mapM loadJobSetForCommit commits
        let names = nub $ map jobName $ concatMap jobsetJobs jobssets

        void $ newLine tout $ T.concat $
            T.replicate (8 + 50) " " :
            map ((" "<>) . fitToLength 7 . textJobName) names

        statuses <- forM jobssets $ \jobset -> do
            let commit = jobsetCommit jobset
                shortCid = T.pack $ take 7 $ showCommitId $ commitId commit
                shortDesc = fitToLength 50 (commitDescription commit)
            case jobsetJobsEither jobset of
                Right jobs -> do
                    outs <- runJobs mngr commit jobs
                    let findJob name = snd <$> find ((name ==) . jobName . fst) outs
                    displayStatusLine tout shortCid (" " <> shortDesc) $ map findJob names
                    return $ map snd outs
                Left err -> do
                    void $ newLine tout $
                        "\ESC[91m" <> shortCid <> "\ESC[0m" <> " " <> shortDesc <> " \ESC[91m" <> T.pack err <> "\ESC[0m"
                    return []

        -- wait for all jobs to complete
        atomically $ forM_ (concat statuses) $ \tvar -> do
            status <- readTVar tvar
            when (not $ jobStatusFinished status) retry


fitToLength :: Int -> Text -> Text
fitToLength maxlen str | len <= maxlen = str <> T.replicate (maxlen - len) " "
                       | otherwise     = T.take (maxlen - 1) str <> "…"
    where len = T.length str

showStatus :: Bool -> JobStatus a -> Text
showStatus blink = \case
    JobQueued       -> "\ESC[94m…\ESC[0m      "
    JobWaiting uses -> "\ESC[94m~" <> fitToLength 6 (T.intercalate "," (map textJobName uses)) <> "\ESC[0m"
    JobSkipped      ->  "\ESC[0m-\ESC[0m      "
    JobRunning      -> "\ESC[96m" <> (if blink then "*" else "•") <> "\ESC[0m      "
    JobError _      -> "\ESC[91m!!\ESC[0m     "
    JobFailed       -> "\ESC[91m✗\ESC[0m      "
    JobDone _       -> "\ESC[92m✓\ESC[0m      "

    JobDuplicate _ s -> case s of
        JobQueued    -> "\ESC[94m^\ESC[0m      "
        JobWaiting _ -> "\ESC[94m^\ESC[0m      "
        JobSkipped   ->  "\ESC[0m-\ESC[0m      "
        JobRunning   -> "\ESC[96m" <> (if blink then "*" else "^") <> "\ESC[0m      "
        _            -> showStatus blink s

displayStatusLine :: TerminalOutput -> Text -> Text -> [ Maybe (TVar (JobStatus JobOutput)) ] -> IO ()
displayStatusLine tout prefix1 prefix2 statuses = do
    line <- newLine tout ""
    void $ forkIO $ do
        go line "\0"
  where
    go line prev = do
        (ss, cur) <- atomically $ do
            ss <- mapM (sequence . fmap readTVar) statuses
            blink <- terminalBlinkStatus tout
            let cur = T.concat $ map (maybe "        " ((" " <>) . showStatus blink)) ss
            when (cur == prev) retry
            return (ss, cur)

        let prefix1' = if any (maybe False jobStatusFailed) ss
                         then "\ESC[91m" <> prefix1 <> "\ESC[0m"
                         else prefix1
        redrawLine line $ prefix1' <> prefix2 <> cur

        if all (maybe True jobStatusFinished) ss
           then return ()
           else go line cur
