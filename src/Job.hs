module Job (
    Job, DeclaredJob, Job'(..),
    JobSet, DeclaredJobSet, JobSet'(..), jobsetJobs,
    JobOutput(..),
    JobName(..), stringJobName, textJobName,
    ArtifactName(..),
    JobStatus(..),
    jobStatusFinished, jobStatusFailed,
    JobManager(..), newJobManager, cancelAllJobs,
    runJobs,
    prepareJob,
    jobStorageSubdir,

    copyRecursive,
    copyRecursiveForce,
) where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class

import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.Directory
import System.Exit
import System.FilePath
import System.FilePath.Glob
import System.IO
import System.IO.Temp
import System.Posix.Signals
import System.Process

import Job.Types
import Output
import Repo


data JobOutput = JobOutput
    { outName :: JobName
    , outArtifacts :: [ArtifactOutput]
    }
    deriving (Eq)

data ArtifactOutput = ArtifactOutput
    { aoutName :: ArtifactName
    , aoutWorkPath :: FilePath
    , aoutStorePath :: FilePath
    }
    deriving (Eq)


data JobStatus a = JobQueued
                 | JobDuplicate JobId (JobStatus a)
                 | JobWaiting [JobName]
                 | JobRunning
                 | JobSkipped
                 | JobError OutputFootnote
                 | JobFailed
                 | JobCancelled
                 | JobDone a
    deriving (Eq)

jobStatusFinished :: JobStatus a -> Bool
jobStatusFinished = \case
    JobQueued      {} -> False
    JobDuplicate _ s  -> jobStatusFinished s
    JobWaiting     {} -> False
    JobRunning     {} -> False
    _                 -> True

jobStatusFailed :: JobStatus a -> Bool
jobStatusFailed = \case
    JobDuplicate _ s  -> jobStatusFailed s
    JobError       {} -> True
    JobFailed      {} -> True
    _                 -> False

textJobStatus :: JobStatus a -> Text
textJobStatus = \case
    JobQueued -> "queued"
    JobDuplicate {} -> "duplicate"
    JobWaiting _ -> "waiting"
    JobRunning -> "running"
    JobSkipped -> "skipped"
    JobError _ -> "error"
    JobFailed -> "failed"
    JobCancelled -> "cancelled"
    JobDone _ -> "done"

textJobStatusDetails :: JobStatus a -> Text
textJobStatusDetails = \case
    JobError err -> footnoteText err <> "\n"
    _ -> ""


data JobManager = JobManager
    { jmSemaphore :: TVar Int
    , jmDataDir :: FilePath
    , jmJobs :: TVar (Map JobId (TVar (JobStatus JobOutput)))
    , jmNextTaskId :: TVar TaskId
    , jmNextTask :: TVar (Maybe TaskId)
    , jmReadyTasks :: TVar (Set TaskId)
    , jmRunningTasks :: TVar (Map TaskId ThreadId)
    , jmCancelled :: TVar Bool
    }

newtype TaskId = TaskId Int
    deriving (Eq, Ord)

data JobCancelledException = JobCancelledException
    deriving (Show)

instance Exception JobCancelledException


newJobManager :: FilePath -> Int -> IO JobManager
newJobManager jmDataDir queueLen = do
    jmSemaphore <- newTVarIO queueLen
    jmJobs <- newTVarIO M.empty
    jmNextTaskId <- newTVarIO (TaskId 0)
    jmNextTask <- newTVarIO Nothing
    jmReadyTasks <- newTVarIO S.empty
    jmRunningTasks <- newTVarIO M.empty
    jmCancelled <- newTVarIO False
    return JobManager {..}

cancelAllJobs :: JobManager -> IO ()
cancelAllJobs JobManager {..} = do
    threads <- atomically $ do
        writeTVar jmCancelled True
        M.elems <$> readTVar jmRunningTasks

    mapM_ (`throwTo` JobCancelledException) threads

reserveTaskId :: JobManager -> STM TaskId
reserveTaskId JobManager {..} = do
    tid@(TaskId n) <- readTVar jmNextTaskId
    writeTVar jmNextTaskId (TaskId (n + 1))
    return tid

runManagedJob :: (MonadIO m, MonadMask m) => JobManager -> TaskId -> m a -> m a -> m a
runManagedJob JobManager {..} tid cancel job = bracket acquire release $ \case
    True -> cancel
    False -> job
  where
    acquire = liftIO $ do
        atomically $ do
            writeTVar jmReadyTasks . S.insert tid =<< readTVar jmReadyTasks
            trySelectNext
        threadId <- myThreadId
        atomically $ do
            readTVar jmCancelled >>= \case
                True -> return True
                False -> readTVar jmNextTask >>= \case
                    Just tid' | tid' == tid -> do
                        writeTVar jmNextTask Nothing
                        writeTVar jmRunningTasks . M.insert tid threadId =<< readTVar jmRunningTasks
                        return False
                    _ -> retry

    release False = liftIO $ atomically $ do
        free <- readTVar jmSemaphore
        writeTVar jmSemaphore $ free + 1
        trySelectNext
    release True = return ()

    trySelectNext = do
        readTVar jmNextTask >>= \case
            Just _ -> return ()
            Nothing -> do
                readTVar jmSemaphore >>= \case
                    0   -> return ()
                    sem -> (S.minView <$> readTVar jmReadyTasks) >>= \case
                        Nothing -> return ()
                        Just ( tid', ready ) -> do
                            writeTVar jmReadyTasks ready
                            writeTVar jmSemaphore (sem - 1)
                            writeTVar jmNextTask (Just tid')
                            writeTVar jmRunningTasks . M.delete tid =<< readTVar jmRunningTasks


runJobs :: JobManager -> Output -> [ Job ] -> IO [ ( Job, TVar (JobStatus JobOutput) ) ]
runJobs mngr@JobManager {..} tout jobs = do
    results <- atomically $ do
        forM jobs $ \job -> do
            tid <- reserveTaskId mngr
            managed <- readTVar jmJobs
            ( job, tid, ) <$> case M.lookup (jobId job) managed of
                Just origVar -> do
                    newTVar . JobDuplicate (jobId job) =<< readTVar origVar

                Nothing -> do
                    statusVar <- newTVar JobQueued
                    writeTVar jmJobs $ M.insert (jobId job) statusVar managed
                    return statusVar

    forM_ results $ \( job, tid, outVar ) -> void $ forkIO $ do
        let handler e = do
                status <- if
                    | Just JobCancelledException <- fromException e -> do
                        return JobCancelled
                    | otherwise -> do
                        JobError <$> outputFootnote tout (T.pack $ displayException e)
                atomically $ writeTVar outVar status
                outputEvent tout $ JobFinished (jobId job) (textJobStatus status)
        handle handler $ do
            res <- runExceptT $ do
                duplicate <- liftIO $ atomically $ do
                    readTVar outVar >>= \case
                        JobDuplicate jid _ -> do
                            fmap ( jid, ) . M.lookup jid <$> readTVar jmJobs
                        _ -> do
                            return Nothing

                case duplicate of
                    Nothing -> do
                        uses <- waitForUsedArtifacts tout job results outVar
                        runManagedJob mngr tid (return JobCancelled) $ do
                            liftIO $ atomically $ writeTVar outVar JobRunning
                            liftIO $ outputEvent tout $ JobStarted (jobId job)
                            prepareJob jmDataDir job $ \checkoutPath jdir -> do
                                updateStatusFile (jdir </> "status") outVar
                                JobDone <$> runJob job uses checkoutPath jdir

                    Just ( jid, origVar ) -> do
                        let wait = do
                                status <- atomically $ do
                                    status <- readTVar origVar
                                    out <- readTVar outVar
                                    if status == out
                                      then retry
                                      else do
                                        writeTVar outVar $ JobDuplicate jid status
                                        return status
                                if jobStatusFinished status
                                  then return $ JobDuplicate jid status
                                  else wait
                        liftIO wait

            atomically $ writeTVar outVar $ either id id res
            outputEvent tout $ JobFinished (jobId job) (textJobStatus $ either id id res)
    return $ map (\( job, _, var ) -> ( job, var )) results

waitForUsedArtifacts :: (MonadIO m, MonadError (JobStatus JobOutput) m) =>
    Output ->
    Job -> [ ( Job, TaskId, TVar (JobStatus JobOutput) ) ] -> TVar (JobStatus JobOutput) -> m [ ArtifactOutput ]
waitForUsedArtifacts tout job results outVar = do
    origState <- liftIO $ atomically $ readTVar outVar
    ujobs <- forM (jobUses job) $ \(ujobName@(JobName tjobName), uartName) -> do
        case find (\( j, _, _ ) -> jobName j == ujobName) results of
            Just ( _, _, var ) -> return ( var, ( ujobName, uartName ))
            Nothing -> throwError . JobError =<< liftIO (outputFootnote tout $ "Job '" <> tjobName <> "' not found")

    let loop prev = do
            ustatuses <- atomically $ do
                ustatuses <- forM ujobs $ \(uoutVar, uartName) -> do
                    (,uartName) <$> readTVar uoutVar
                when (Just (map fst ustatuses) == prev) retry
                let remains = map (fst . snd) $ filter (not . jobStatusFinished . fst) ustatuses
                writeTVar outVar $ if null remains then origState else JobWaiting remains
                return ustatuses
            if all (jobStatusFinished . fst) ustatuses
               then return ustatuses
               else loop $ Just $ map fst ustatuses
    ustatuses <- liftIO $ loop Nothing

    forM ustatuses $ \(ustatus, (JobName tjobName, uartName@(ArtifactName tartName))) -> do
        case ustatus of
            JobDone out -> case find ((==uartName) . aoutName) $ outArtifacts out of
                Just art -> return art
                Nothing -> throwError . JobError =<< liftIO (outputFootnote tout $ "Artifact '" <> tjobName <> "." <> tartName <> "' not found")
            _ -> throwError JobSkipped

updateStatusFile :: MonadIO m => FilePath -> TVar (JobStatus JobOutput) -> m ()
updateStatusFile path outVar = void $ liftIO $ forkIO $ loop Nothing
  where
    loop prev = do
        status <- atomically $ do
            status <- readTVar outVar
            when (Just status == prev) retry
            return status
        T.writeFile path $ textJobStatus status <> "\n" <> textJobStatusDetails status
        when (not (jobStatusFinished status)) $ loop $ Just status

jobStorageSubdir :: JobId -> FilePath
jobStorageSubdir (JobId jidParts) = "jobs" </> joinPath (map (T.unpack . textJobIdPart) (jidParts))

prepareJob :: (MonadIO m, MonadMask m, MonadFail m) => FilePath -> Job -> (FilePath -> FilePath -> m a) -> m a
prepareJob dir job inner = do
    withSystemTempDirectory "minici" $ \checkoutPath -> do
        forM_ (jobCheckout job) $ \(JobCheckout tree mbsub dest) -> do
            subtree <- maybe return (getSubtree Nothing . makeRelative (treeSubdir tree)) mbsub $ tree
            checkoutAt subtree $ checkoutPath </> fromMaybe "" dest

        let jdir = dir </> jobStorageSubdir (jobId job)
        liftIO $ createDirectoryIfMissing True jdir
        inner checkoutPath jdir

runJob :: Job -> [ArtifactOutput] -> FilePath -> FilePath -> ExceptT (JobStatus JobOutput) IO JobOutput
runJob job uses checkoutPath jdir = do
    liftIO $ forM_ uses $ \aout -> do
        let target = checkoutPath </> aoutWorkPath aout
        createDirectoryIfMissing True $ takeDirectory target
        copyRecursive (aoutStorePath aout) target

    bracket (liftIO $ openFile (jdir </> "log") WriteMode) (liftIO . hClose) $ \logs -> do
        forM_ (jobRecipe job) $ \p -> do
            (Just hin, _, _, hp) <- liftIO $ createProcess_ "" p
                { cwd = Just checkoutPath
                , std_in = CreatePipe
                , std_out = UseHandle logs
                , std_err = UseHandle logs
                }
            liftIO $ hClose hin
            liftIO (waitForProcess hp) >>= \case
                ExitSuccess -> return ()
                ExitFailure n
                    | fromIntegral n == -sigINT -> throwError JobCancelled
                    | otherwise -> throwError JobFailed

        let adir = jdir </> "artifacts"
        artifacts <- forM (jobArtifacts job) $ \( name@(ArtifactName tname), pathPattern ) -> do
            path <- liftIO (globDir1 pathPattern checkoutPath) >>= \case
                [ path ] -> return path
                found -> do
                    liftIO $ hPutStrLn logs $
                        (if null found then "no file" else "multiple files") <> " found matching pattern ‘" <>
                        decompile pathPattern <> "’ for artifact ‘" <> T.unpack tname <> "’"
                    throwError JobFailed
            let target = adir </> T.unpack tname </> takeFileName path
            liftIO $ do
                createDirectoryIfMissing True $ takeDirectory target
                copyRecursiveForce path target
            return $ ArtifactOutput
                { aoutName = name
                , aoutWorkPath = makeRelative checkoutPath path
                , aoutStorePath = target
                }

        return JobOutput
            { outName = jobName job
            , outArtifacts = artifacts
            }


copyRecursive :: FilePath -> FilePath -> IO ()
copyRecursive from to = do
    doesDirectoryExist from >>= \case
        False -> do
            copyFile from to
        True -> do
            createDirectory to
            content <- listDirectory from
            forM_ content $ \name -> do
                copyRecursive  (from </> name) (to </> name)

copyRecursiveForce :: FilePath -> FilePath -> IO ()
copyRecursiveForce from to = do
    doesDirectoryExist to >>= \case
        False -> return ()
        True  -> removeDirectoryRecursive to
    copyRecursive from to
