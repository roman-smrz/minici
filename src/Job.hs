module Job (
    Job(..),
    JobOutput(..),
    JobName(..), stringJobName, textJobName,
    ArtifactName(..),
    JobStatus(..),
    jobStatusFinished, jobStatusFailed,
    runJobs,
) where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except

import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

data Job = Job
    { jobName :: JobName
    , jobRecipe :: [CreateProcess]
    , jobArtifacts :: [(ArtifactName, CreateProcess)]
    , jobUses :: [(JobName, ArtifactName)]
    }

data JobOutput = JobOutput
    { outName :: JobName
    , outArtifacts :: [ArtifactOutput]
    }
    deriving (Eq)

data JobName = JobName Text
    deriving (Eq, Ord, Show)

stringJobName :: JobName -> String
stringJobName (JobName name) = T.unpack name

textJobName :: JobName -> Text
textJobName (JobName name) = name

data ArtifactName = ArtifactName Text
    deriving (Eq, Ord, Show)

data ArtifactOutput = ArtifactOutput
    { aoutName :: ArtifactName
    , aoutWorkPath :: FilePath
    , aoutStorePath :: FilePath
    }
    deriving (Eq)


data JobStatus a = JobQueued
                 | JobWaiting [JobName]
                 | JobRunning
                 | JobSkipped
                 | JobError Text
                 | JobFailed
                 | JobDone a
    deriving (Eq)

jobStatusFinished :: JobStatus a -> Bool
jobStatusFinished = \case
    JobQueued  {} -> False
    JobWaiting {} -> False
    JobRunning {} -> False
    _             -> True

jobStatusFailed :: JobStatus a -> Bool
jobStatusFailed = \case
    JobError  {} -> True
    JobFailed {} -> True
    _            -> False

textJobStatus :: JobStatus a -> Text
textJobStatus = \case
    JobQueued -> "queued"
    JobWaiting _ -> "waiting"
    JobRunning -> "running"
    JobSkipped -> "skipped"
    JobError err -> "error\n" <> err
    JobFailed -> "failed"
    JobDone _ -> "done"


runJobs :: FilePath -> String -> [Job] -> IO [TVar (JobStatus JobOutput)]
runJobs dir cid jobs = do
    results <- forM jobs $ \job -> (job,) <$> newTVarIO JobQueued
    gitLock <- newMVar ()
    forM_ results $ \(job, outVar) -> void $ forkIO $ do
        res <- runExceptT $ do
            uses <- waitForUsedArtifacts job results outVar
            liftIO $ atomically $ writeTVar outVar JobRunning
            prepareJob gitLock dir cid job $ \checkoutPath jdir -> do
                updateStatusFile (jdir </> "status") outVar
                runJob job uses checkoutPath jdir

        case res of
            Left (JobError err) -> T.putStrLn err
            _ -> return ()

        atomically $ writeTVar outVar $ either id JobDone res
    return $ map snd results

waitForUsedArtifacts :: (MonadIO m, MonadError (JobStatus JobOutput) m) =>
    Job -> [(Job, TVar (JobStatus JobOutput))] -> TVar (JobStatus JobOutput) -> m [ArtifactOutput]
waitForUsedArtifacts job results outVar = do
    ujobs <- forM (jobUses job) $ \(ujobName@(JobName tjobName), uartName) -> do
        case find ((==ujobName) . jobName . fst) results of
            Just (_, var) -> return (var, (ujobName, uartName))
            Nothing -> throwError $ JobError $ "Job '" <> tjobName <> "' not found"

    let loop prev = do
            ustatuses <- atomically $ do
                ustatuses <- forM ujobs $ \(uoutVar, uartName) -> do
                    (,uartName) <$> readTVar uoutVar
                when (Just (map fst ustatuses) == prev) retry
                writeTVar outVar $ JobWaiting $ map (fst . snd) $ filter (not . jobStatusFinished . fst) ustatuses
                return ustatuses
            if all (jobStatusFinished . fst) ustatuses
               then return ustatuses
               else loop $ Just $ map fst ustatuses
    ustatuses <- liftIO $ loop Nothing

    forM ustatuses $ \(ustatus, (JobName tjobName, uartName@(ArtifactName tartName))) -> do
        case ustatus of
            JobDone out -> case find ((==uartName) . aoutName) $ outArtifacts out of
                Just art -> return art
                Nothing -> throwError $ JobError $ "Artifact '" <> tjobName <> "." <> tartName <> "' not found"
            _ -> throwError JobSkipped

updateStatusFile :: MonadIO m => FilePath -> TVar (JobStatus JobOutput) -> m ()
updateStatusFile path outVar = void $ liftIO $ forkIO $ loop Nothing
  where
    loop prev = do
        status <- atomically $ do
            status <- readTVar outVar
            when (Just status == prev) retry
            return status
        T.writeFile path $ textJobStatus status <> "\n"
        when (not (jobStatusFinished status)) $ loop $ Just status

prepareJob :: (MonadIO m, MonadMask m, MonadFail m) => MVar () -> FilePath -> String -> Job -> (FilePath -> FilePath -> m a) -> m a
prepareJob gitLock dir cid job inner = do
    [checkoutPath] <- fmap lines $ liftIO $
        readProcess "mktemp" ["-d", "-t", "minici.XXXXXXXXXX"] ""

    flip finally (liftIO $ removeDirectoryRecursive checkoutPath) $ do
        tid <- liftIO $ withMVar gitLock $ \_ -> do
            "" <- readProcess "git" ["--work-tree=" <> checkoutPath, "restore", "--source=" <> cid, "--", "."] ""
            ["tree", tid]:_ <- map words . lines <$> readProcess "git" ["cat-file", "-p", cid] ""
            return tid

        let jdir = dir </> "jobs" </> tid </> stringJobName (jobName job)
        liftIO $ createDirectoryIfMissing True jdir

        inner checkoutPath jdir

runJob :: Job -> [ArtifactOutput] -> FilePath -> FilePath -> ExceptT (JobStatus JobOutput) IO JobOutput
runJob job uses checkoutPath jdir = do
    liftIO $ forM_ uses $ \aout -> do
        let target = checkoutPath </> aoutWorkPath aout
        createDirectoryIfMissing True $ takeDirectory target
        copyFile (aoutStorePath aout) target

    bracket (liftIO $ openFile (jdir </> "log") WriteMode) (liftIO . hClose) $ \logs -> do
        forM_ (jobRecipe job) $ \p -> do
            (Just hin, _, _, hp) <- liftIO $ createProcess_ "" p
                { cwd = Just checkoutPath
                , std_in = CreatePipe
                , std_out = UseHandle logs
                , std_err = UseHandle logs
                }
            liftIO $ hClose hin
            exit <- liftIO $ waitForProcess hp

            when (exit /= ExitSuccess) $
                throwError JobFailed

    let adir = jdir </> "artifacts"
    artifacts <- forM (jobArtifacts job) $ \(name@(ArtifactName tname), pathCmd) -> liftIO $ do
        [path] <- lines <$> readCreateProcess pathCmd { cwd = Just checkoutPath } ""
        let target = adir </> T.unpack tname
        createDirectoryIfMissing True adir
        copyFile (checkoutPath </> path) target
        return $ ArtifactOutput
            { aoutName = name
            , aoutWorkPath = path
            , aoutStorePath = target
            }

    return JobOutput
        { outName = jobName job
        , outArtifacts = artifacts
        }
