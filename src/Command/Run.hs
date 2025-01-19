module Command.Run (
    RunCommand,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Reader

import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.Console.GetOpt
import System.Exit
import System.IO
import System.Process

import Command
import Config
import Job
import Repo
import Terminal


data RunCommand = RunCommand RunOptions [ Text ]

data RunOptions = RunOptions
    { roRanges :: [ Text ]
    , roNewCommitsOn :: [ Text ]
    }

instance Command RunCommand where
    commandName _ = "run"
    commandDescription _ = "Execude jobs per minici.yaml for given commits"

    type CommandArguments RunCommand = [ Text ]

    commandUsage _ = T.pack $ unlines $
        [ "Usage: minici run"
        , "         run jobs for commits on current branch not yet in upstream branch"
        , "   or: minici run <ref>"
        , "         run jobs for commits on <ref> not yet in its upstream ref"
        , "   or: minici run <commit>..<commit>"
        , "         run jobs for commits in given range"
        , "   or: minici run <option>..."
        , "         run jobs based on given options (see below)"
        ]

    type CommandOptions RunCommand = RunOptions
    defaultCommandOptions _ = RunOptions
        { roRanges = []
        , roNewCommitsOn = []
        }

    commandOptions _ =
        [ Option [] [ "range" ]
            (ReqArg (\val opts -> opts { roRanges = T.pack val : roRanges opts }) "<range>")
            "run jobs for commits in given range"
        , Option [] [ "new-commits-on" ]
            (ReqArg (\val opts -> opts { roNewCommitsOn = T.pack val : roNewCommitsOn opts }) "<branch>")
            "run jobs for new commits on given branch"
        ]

    commandInit _ = RunCommand
    commandExec = cmdRun


data JobSource = JobSource (TMVar (Maybe ( [ JobSet ], JobSource )))

takeJobSource :: JobSource -> STM (Maybe ( [ JobSet ], JobSource ))
takeJobSource (JobSource tmvar) = takeTMVar tmvar

mergeSources :: [ JobSource ] -> IO JobSource
mergeSources sources = do
    let go tmvar [] = do
            atomically (putTMVar tmvar Nothing)
        go tmvar cur = do
            ( jobsets, next ) <- atomically (select cur)
            if null next
              then do
                go tmvar next
              else do
                nextvar <- newEmptyTMVarIO
                atomically $ putTMVar tmvar (Just ( jobsets, JobSource nextvar ))
                go nextvar next

    tmvar <- newEmptyTMVarIO
    void $ forkIO $ go tmvar sources
    return $ JobSource tmvar

  where
    select :: [ JobSource ] -> STM ( [ JobSet ], [ JobSource ] )
    select [] = retry
    select (x@(JobSource tmvar) : xs) = do
        tryTakeTMVar tmvar >>= \case
            Nothing -> fmap (x :) <$> select xs
            Just Nothing -> return ( [], xs )
            Just (Just ( jobsets, x' )) -> return ( jobsets, x' : xs )


rangeSource :: Repo -> Text -> Text -> IO JobSource
rangeSource repo base tip = do
    commits <- listCommits repo (base <> ".." <> tip)
    jobsets <- mapM loadJobSetForCommit commits
    next <- JobSource <$> newTMVarIO Nothing
    JobSource <$> newTMVarIO (Just ( jobsets, next ))

watchBranchSource :: Repo -> Text -> IO JobSource
watchBranchSource repo branch = do
    getCurrentTip <- watchBranch repo branch
    let go prev tmvar = do
            cur <- atomically $ do
                getCurrentTip >>= \case
                    Just cur -> do
                        when (cur == prev) retry
                        return cur
                    Nothing -> retry

            commits <- listCommits repo (textCommitId (commitId prev) <> ".." <> textCommitId (commitId cur))
            jobsets <- mapM loadJobSetForCommit commits
            nextvar <- newEmptyTMVarIO
            atomically $ putTMVar tmvar $ Just ( jobsets, JobSource nextvar )
            go cur nextvar

    tmvar <- newEmptyTMVarIO
    atomically getCurrentTip >>= \case
        Just commit -> 
            void $ forkIO $ go commit tmvar
        Nothing -> do
            T.hPutStrLn stderr $ "Branch `" <> branch <> "' not found"
            atomically $ putTMVar tmvar Nothing
    return $ JobSource tmvar

cmdRun :: RunCommand -> CommandExec ()
cmdRun (RunCommand RunOptions {..} args) = do
    CommonOptions {..} <- getCommonOptions
    tout <- getTerminalOutput

    liftIO $ do
        Just repo <- openRepo "."
        ranges <- forM (args ++ roRanges) $ \changeset -> do
            ( base, tip ) <- case T.splitOn ".." changeset of
                base : tip : _ -> return ( base, tip )
                [ param ] -> liftIO $ do
                    [ deref ] <- readProcessWithExitCode "git" [ "symbolic-ref", "--quiet", T.unpack param ] "" >>= \case
                        ( ExitSuccess, out, _ ) -> return $ lines out
                        ( _, _, _ ) -> return [ T.unpack param ]
                    [ _, tip ] : _ <- fmap words . lines <$> readProcess "git" [ "show-ref", deref ] ""
                    [ base ] <- lines <$> readProcess "git" [ "for-each-ref", "--format=%(upstream)", tip ] ""
                    return ( T.pack base, T.pack tip )
                [] -> error "splitOn should not return empty list"
            rangeSource repo base tip

        branches <- mapM (watchBranchSource repo) roNewCommitsOn

        mngr <- newJobManager "./.minici" optJobs

        source <- mergeSources $ concat [ ranges, branches ]
        headerLine <- newLine tout ""

        threadCount <- newTVarIO (0 :: Int)
        let changeCount f = atomically $ do
                writeTVar threadCount . f =<< readTVar threadCount
        let waitForJobs = atomically $ do
                flip when retry . (0 <) =<< readTVar threadCount

        let loop _ Nothing = return ()
            loop names (Just ( [], next )) = do
                loop names =<< atomically (takeJobSource next)

            loop pnames (Just ( jobset : rest, next )) = do
                let names = nub $ (pnames ++) $ map jobName $ jobsetJobs jobset
                when (names /= pnames) $ do
                    redrawLine headerLine $ T.concat $
                        T.replicate (8 + 50) " " :
                        map ((" " <>) . fitToLength 7 . textJobName) names

                let commit = jobsetCommit jobset
                    shortCid = T.pack $ take 7 $ showCommitId $ commitId commit
                shortDesc <- fitToLength 50 <$> getCommitTitle commit

                case jobsetJobsEither jobset of
                    Right jobs -> do
                        outs <- runJobs mngr commit jobs
                        let findJob name = snd <$> find ((name ==) . jobName . fst) outs
                        line <- newLine tout ""
                        mask $ \restore -> do
                            changeCount (+ 1)
                            void $ forkIO $ (>> changeCount (subtract 1)) $
                                try @SomeException $ restore $ do
                                    displayStatusLine tout line shortCid (" " <> shortDesc) $ map findJob names
                    Left err -> do
                        void $ newLine tout $
                            "\ESC[91m" <> shortCid <> "\ESC[0m" <> " " <> shortDesc <> " \ESC[91m" <> T.pack err <> "\ESC[0m"
                loop names (Just ( rest, next ))

        handle @SomeException (\_ -> cancelAllJobs mngr) $ do
            loop [] =<< atomically (takeJobSource source)
            waitForJobs
        waitForJobs


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
    JobCancelled    ->  "\ESC[0mC\ESC[0m      "
    JobDone _       -> "\ESC[92m✓\ESC[0m      "

    JobDuplicate _ s -> case s of
        JobQueued    -> "\ESC[94m^\ESC[0m      "
        JobWaiting _ -> "\ESC[94m^\ESC[0m      "
        JobSkipped   ->  "\ESC[0m-\ESC[0m      "
        JobRunning   -> "\ESC[96m" <> (if blink then "*" else "^") <> "\ESC[0m      "
        _            -> showStatus blink s

displayStatusLine :: TerminalOutput -> TerminalLine -> Text -> Text -> [ Maybe (TVar (JobStatus JobOutput)) ] -> IO ()
displayStatusLine tout line prefix1 prefix2 statuses = do
    go "\0"
  where
    go prev = do
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
           else go cur
