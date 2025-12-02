module Command.Run (
    RunCommand,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.Either
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.Console.GetOpt
import System.FilePath.Glob
import System.IO

import Command
import Config
import Eval
import Job
import Job.Types
import Output
import Repo
import Terminal


data RunCommand = RunCommand RunOptions [ Text ]

data RunOptions = RunOptions
    { roRerun :: RerunOption
    , roRanges :: [ Text ]
    , roSinceUpstream :: [ Text ]
    , roNewCommitsOn :: [ Text ]
    , roNewTags :: [ Pattern ]
    }

data RerunOption
    = RerunExplicit
    | RerunFailed
    | RerunAll
    | RerunNone

instance Command RunCommand where
    commandName _ = "run"
    commandDescription _ = "Execude jobs per minici.yaml for given commits"

    type CommandArguments RunCommand = [ Text ]

    commandUsage _ = T.pack $ unlines $
        [ "Usage: minici run"
        , "         run jobs for commits on current branch not yet in upstream branch"
        , "   or: minici run <job>..."
        , "         run jobs specified on the command line"
        , "   or: minici run [--range=]<commit>..<commit>"
        , "         run jobs for commits in given range"
        , "   or: minici run <option>..."
        , "         run jobs based on given options (see below)"
        ]

    type CommandOptions RunCommand = RunOptions
    defaultCommandOptions _ = RunOptions
        { roRerun = RerunExplicit
        , roRanges = []
        , roSinceUpstream = []
        , roNewCommitsOn = []
        , roNewTags = []
        }

    commandOptions _ =
        [ Option [] [ "rerun-explicit" ]
            (NoArg (\opts -> opts { roRerun = RerunExplicit }))
            "rerun jobs given explicitly on command line and their failed dependencies (default)"
        , Option [] [ "rerun-failed" ]
            (NoArg (\opts -> opts { roRerun = RerunFailed }))
            "rerun failed jobs only"
        , Option [] [ "rerun-all" ]
            (NoArg (\opts -> opts { roRerun = RerunAll }))
            "rerun all jobs"
        , Option [] [ "rerun-none" ]
            (NoArg (\opts -> opts { roRerun = RerunNone }))
            "do not rerun any job"
        , Option [] [ "range" ]
            (ReqArg (\val opts -> opts { roRanges = T.pack val : roRanges opts }) "<range>")
            "run jobs for commits in given range"
        , Option [] [ "since-upstream" ]
            (ReqArg (\val opts -> opts { roSinceUpstream = T.pack val : roSinceUpstream opts }) "<ref>")
            "run jobs for commits on <ref> not yet in its upstream ref"
        , Option [] [ "new-commits-on" ]
            (ReqArg (\val opts -> opts { roNewCommitsOn = T.pack val : roNewCommitsOn opts }) "<branch>")
            "run jobs for new commits on given branch"
        , Option [] [ "new-tags" ]
            (ReqArg (\val opts -> opts { roNewTags = compile val : roNewTags opts }) "<pattern>")
            "run jobs for new annotated tags matching pattern"
        ]

    commandInit _ = RunCommand
    commandExec = cmdRun


data JobSource = JobSource (TMVar (Maybe ( [ JobSet ], JobSource )))

emptyJobSource :: MonadIO m => m JobSource
emptyJobSource = JobSource <$> liftIO (newTMVarIO Nothing)

oneshotJobSource :: MonadIO m => [ JobSet ] -> m JobSource
oneshotJobSource jobsets = do
    next <- emptyJobSource
    JobSource <$> liftIO (newTMVarIO (Just ( jobsets, next )))

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


argumentJobSource :: [ JobName ] -> CommandExec JobSource
argumentJobSource [] = emptyJobSource
argumentJobSource names = do
    jobRoot <- getJobRoot
    ( config, jcommit ) <- case jobRoot of
        JobRootConfig config -> do
            commit <- sequence . fmap createWipCommit =<< tryGetDefaultRepo
            return ( config, commit )
        JobRootRepo repo -> do
            commit <- createWipCommit repo
            config <- either fail return =<< loadConfigForCommit =<< getCommitTree commit
            return ( config, Just commit )

    jobtree <- case jcommit of
        Just commit -> (: []) <$> getCommitTree commit
        Nothing -> return []
    let cidPart = case jobRoot of
            JobRootConfig {} -> []
            JobRootRepo {} -> map (JobIdTree Nothing "" . treeId) jobtree
    forM_ names $ \name ->
        case find ((name ==) . jobName) (configJobs config) of
            Just _  -> return ()
            Nothing -> tfail $ "job ‘" <> textJobName name <> "’ not found"

    jset <- cmdEvalWith (\ei -> ei { eiCurrentIdRev = cidPart ++ eiCurrentIdRev ei }) $ do
        fullSet <- evalJobSet (map ( Nothing, ) jobtree) JobSet
            { jobsetId = ()
            , jobsetConfig = Just config
            , jobsetCommit = jcommit
            , jobsetExplicitlyRequested = names
            , jobsetJobsEither = Right (configJobs config)
            }
        let selectedSet = fullSet { jobsetJobsEither = fmap (filter ((`elem` names) . jobName)) (jobsetJobsEither fullSet) }
        fillInDependencies selectedSet
    oneshotJobSource [ jset ]

refJobSource :: [ JobRef ] -> CommandExec JobSource
refJobSource [] = emptyJobSource
refJobSource refs = do
    jsets <- foldl' addJobToList [] <$> cmdEvalWith id (mapM evalJobReference refs)
    sets <- cmdEvalWith id $ do
        forM jsets $ \jset -> do
            fillInDependencies $ jset { jobsetExplicitlyRequested = either (const []) (map jobId) $ jobsetJobsEither jset }
    oneshotJobSource sets
  where
    addJobToList :: [ JobSet ] -> JobSet -> [ JobSet ]
    addJobToList (cur : rest) jset
        | jobsetId cur == jobsetId jset = cur { jobsetJobsEither = (++) <$> (fmap reverse $ jobsetJobsEither jset) <*> (jobsetJobsEither cur) } : rest
        | otherwise                     = cur : addJobToList rest jset
    addJobToList [] jset                = [ jset ]

loadJobSetFromRoot :: (MonadIO m, MonadFail m) => JobRoot -> Commit -> m DeclaredJobSet
loadJobSetFromRoot root commit = case root of
    JobRootRepo _ -> loadJobSetForCommit commit
    JobRootConfig config -> return JobSet
        { jobsetId = ()
        , jobsetConfig = Just config
        , jobsetCommit = Just commit
        , jobsetExplicitlyRequested = []
        , jobsetJobsEither = Right $ configJobs config
        }

rangeSource :: Text -> Text -> CommandExec JobSource
rangeSource base tip = do
    root <- getJobRoot
    repo <- getDefaultRepo
    commits <- listCommits repo (base <> ".." <> tip)
    jobsets <- forM commits $ \commit -> do
        tree <- getCommitTree commit
        cmdEvalWith (\ei -> ei
            { eiCurrentIdRev = JobIdTree Nothing (treeSubdir tree) (treeId tree) : eiCurrentIdRev ei
            }) . evalJobSet [ ( Nothing, tree) ] =<< loadJobSetFromRoot root commit
    oneshotJobSource jobsets


watchBranchSource :: Text -> CommandExec JobSource
watchBranchSource branch = do
    root <- getJobRoot
    repo <- getDefaultRepo
    einputBase <- getEvalInput
    getCurrentTip <- watchBranch repo branch
    let go prev tmvar = do
            cur <- atomically $ do
                getCurrentTip >>= \case
                    Just cur -> do
                        when (cur == prev) retry
                        return cur
                    Nothing -> retry

            commits <- listCommits repo (textCommitId (commitId prev) <> ".." <> textCommitId (commitId cur))
            jobsets <- forM commits $ \commit -> do
                tree <- getCommitTree commit
                let einput = einputBase
                        { eiCurrentIdRev = JobIdTree Nothing (treeSubdir tree) (treeId tree) : eiCurrentIdRev einputBase
                        }
                either (fail . T.unpack . textEvalError) return =<<
                    flip runEval einput . evalJobSet [ ( Nothing, tree ) ] =<< loadJobSetFromRoot root commit
            nextvar <- newEmptyTMVarIO
            atomically $ putTMVar tmvar $ Just ( jobsets, JobSource nextvar )
            go cur nextvar

    liftIO $ do
        tmvar <- newEmptyTMVarIO
        atomically getCurrentTip >>= \case
            Just commit ->
                void $ forkIO $ go commit tmvar
            Nothing -> do
                T.hPutStrLn stderr $ "Branch ‘" <> branch <> "’ not found"
                atomically $ putTMVar tmvar Nothing
        return $ JobSource tmvar

watchTagSource :: Pattern -> CommandExec JobSource
watchTagSource pat = do
    root <- getJobRoot
    chan <- watchTags =<< getDefaultRepo
    einputBase <- getEvalInput

    let go tmvar = do
            tag <- atomically $ readTChan chan
            if match pat $ T.unpack $ tagTag tag
              then do
                tree <- getCommitTree $ tagObject tag
                let einput = einputBase
                        { eiCurrentIdRev = JobIdTree Nothing (treeSubdir tree) (treeId tree) : eiCurrentIdRev einputBase
                        }
                jobset <- either (fail . T.unpack . textEvalError) return =<<
                    flip runEval einput . evalJobSet [ ( Nothing, tree ) ] =<< loadJobSetFromRoot root (tagObject tag)
                nextvar <- newEmptyTMVarIO
                atomically $ putTMVar tmvar $ Just ( [ jobset ], JobSource nextvar )
                go nextvar
              else do
                go tmvar

    liftIO $ do
        tmvar <- newEmptyTMVarIO
        void $ forkIO $ go tmvar
        return $ JobSource tmvar

cmdRun :: RunCommand -> CommandExec ()
cmdRun (RunCommand RunOptions {..} args) = do
    CommonOptions {..} <- getCommonOptions
    output <- getOutput
    storageDir <- getStorageDir

    ( rangeOptions, jobOptions ) <- partitionEithers . concat <$> sequence
        [ forM roRanges $ \range -> case T.splitOn ".." range of
            [ base, tip ]
                | not (T.null base) && not (T.null tip)
                -> return $ Left ( Just base, tip )
            _ -> tfail $ "invalid range: " <> range
        , forM roSinceUpstream $ return . Left . ( Nothing, )
        , forM args $ \arg -> case T.splitOn ".." arg of
            [ base, tip ]
                | not (T.null base) && not (T.null tip)
                -> return $ Left ( Just base, tip )
            [ _ ] -> return $ Right arg
            _ -> tfail $ "invalid argument: " <> arg
        ]

    let ( refOptions, nameOptions ) = partition (T.elem '.') jobOptions

    argumentJobs <- argumentJobSource $ map JobName nameOptions
    refJobs <- refJobSource $ map parseJobRef refOptions

    defaultSource <- getJobRoot >>= \case
        _ | not (null rangeOptions && null roNewCommitsOn && null roNewTags && null jobOptions) -> do
            emptyJobSource

        JobRootRepo repo -> do
            Just base <- findUpstreamRef repo "HEAD"
            rangeSource base "HEAD"

        JobRootConfig config -> do
            argumentJobSource (jobName <$> configJobs config)

    ranges <- forM rangeOptions $ \( mbBase, paramTip ) -> do
        ( base, tip ) <- case mbBase of
            Just base -> return ( base, paramTip )
            Nothing -> do
                Just base <- flip findUpstreamRef paramTip =<< getDefaultRepo
                return ( base, paramTip )
        rangeSource base tip

    branches <- mapM watchBranchSource roNewCommitsOn
    tags <- mapM watchTagSource roNewTags

    liftIO $ do
        mngr <- newJobManager storageDir optJobs

        source <- mergeSources $ concat [ [ defaultSource, argumentJobs, refJobs ], ranges, branches, tags ]
        mbHeaderLine <- mapM (flip newLine "") (outputTerminal output)

        threadCount <- newTVarIO (0 :: Int)
        let changeCount f = atomically $ do
                writeTVar threadCount . f =<< readTVar threadCount
        let waitForJobs = do
                atomically $ do
                    flip when retry . (0 <) =<< readTVar threadCount
                waitForRemainingTasks mngr

        let loop _ Nothing = return ()
            loop names (Just ( [], next )) = do
                loop names =<< atomically (takeJobSource next)

            loop pnames (Just ( jobset : rest, next )) = do
                let names = nub $ (pnames ++) $ map jobName $ jobsetJobs jobset
                when (names /= pnames) $ do
                    forM_ mbHeaderLine $ \headerLine -> do
                        redrawLine headerLine $ T.concat $
                            T.replicate (8 + 50) " " :
                            map ((" " <>) . fitToLength 7 . textJobName) names

                let commit = jobsetCommit jobset
                    shortCid = T.pack $ take 7 $ maybe (repeat ' ') (showCommitId . commitId) commit
                shortDesc <- fitToLength 50 <$> maybe (return "") getCommitTitle commit

                case jobsetJobsEither jobset of
                    Right jobs -> do
                        outs <- runJobs mngr output jobs $ case roRerun of
                            RerunExplicit -> \jid status -> jid `elem` jobsetExplicitlyRequested jobset || jobStatusFailed status
                            RerunFailed -> \_ status -> jobStatusFailed status
                            RerunAll -> \_ _ -> True
                            RerunNone -> \_ _ -> False
                        let findJob name = snd <$> find ((name ==) . jobName . fst) outs
                            statuses = map findJob names
                        forM_ (outputTerminal output) $ \tout -> do
                            line <- newLine tout ""
                            void $ forkIO $ do
                                displayStatusLine tout line shortCid (" " <> shortDesc) statuses
                        mask $ \restore -> do
                            changeCount (+ 1)
                            void $ forkIO $ do
                                void $ try @SomeException $ restore $ waitForJobStatuses statuses
                                changeCount (subtract 1)
                    Left err -> do
                        forM_ (outputTerminal output) $ flip newLine $
                            "\ESC[91m" <> shortCid <> "\ESC[0m" <> " " <> shortDesc <> " \ESC[91m" <> T.pack err <> "\ESC[0m"
                        outputEvent output $ TestMessage $ "jobset-fail " <> T.pack err
                        outputEvent output $ LogMessage $ "Jobset failed: " <> shortCid <> " " <> T.pack err
                loop names (Just ( rest, next ))

        handle @SomeException (\_ -> cancelAllJobs mngr) $ do
            loop [] =<< atomically (takeJobSource source)
            waitForJobs
        waitForJobs
    outputEvent output $ TestMessage "run-finish"


fitToLength :: Int -> Text -> Text
fitToLength maxlen str | len <= maxlen = str <> T.replicate (maxlen - len) " "
                       | otherwise     = T.take (maxlen - 1) str <> "…"
    where len = T.length str

showStatus :: Bool -> JobStatus a -> Text
showStatus blink = \case
    JobQueued       -> " \ESC[94m…\ESC[0m     "
    JobWaiting uses -> "\ESC[94m~" <> fitToLength 6 (T.intercalate "," (map textJobName uses)) <> "\ESC[0m"
    JobSkipped      ->  " \ESC[0m-\ESC[0m     "
    JobRunning      -> " \ESC[96m" <> (if blink then "*" else "•") <> "\ESC[0m     "
    JobError fnote  -> "\ESC[91m" <> fitToLength 7 ("!! [" <> T.pack (maybe "?" (show . tfNumber) (footnoteTerminal fnote)) <> "]") <> "\ESC[0m"
    JobFailed       -> " \ESC[91m✗\ESC[0m     "
    JobCancelled    ->  " \ESC[0mC\ESC[0m     "
    JobDone _       -> " \ESC[92m✓\ESC[0m     "

    JobDuplicate _ s -> case s of
        JobQueued    -> " \ESC[94m^\ESC[0m     "
        JobWaiting _ -> " \ESC[94m^\ESC[0m     "
        JobSkipped   ->  " \ESC[0m-\ESC[0m     "
        JobRunning   -> " \ESC[96m" <> (if blink then "*" else "^") <> "\ESC[0m     "
        _            -> showStatus blink s

    JobPreviousStatus (JobDone _) -> "\ESC[90m«\ESC[32m✓\ESC[0m     "
    JobPreviousStatus (JobFailed) -> "\ESC[90m«\ESC[31m✗\ESC[0m     "
    JobPreviousStatus s           -> "\ESC[90m«" <> T.init (showStatus blink s)

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

waitForJobStatuses :: [ Maybe (TVar (JobStatus a)) ] -> IO ()
waitForJobStatuses mbstatuses = do
    let statuses = catMaybes mbstatuses
    atomically $ do
        ss <- mapM readTVar statuses
        when (any (not . jobStatusFinished) ss) retry
