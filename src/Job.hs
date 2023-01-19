module Job (
    Job(..),
    JobOutput(..),
    JobName(..), stringJobName,
    ArtifactName(..),
    runJobs,
) where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.Except

import Data.List
import Data.Text (Text)
import Data.Text qualified as T

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
    , outStatus :: Bool
    , outArtifacts :: [ArtifactOutput]
    }

data JobName = JobName Text
    deriving (Eq, Ord, Show)

stringJobName :: JobName -> String
stringJobName (JobName name) = T.unpack name

data ArtifactName = ArtifactName Text
    deriving (Eq, Ord, Show)

data ArtifactOutput = ArtifactOutput
    { aoutName :: ArtifactName
    , aoutWorkPath :: FilePath
    , aoutStorePath :: FilePath
    }


runJobs :: FilePath -> String -> [Job] -> IO [TVar (Maybe JobOutput)]
runJobs dir cid jobs = do
    results <- forM jobs $ \job -> (job,) <$> newTVarIO Nothing
    gitLock <- newMVar ()
    forM_ results $ \(job, outVar) -> void $ forkIO $ do
        uses <- forM (jobUses job) $ \(ujobName, uartName) -> do
            Just (_, uoutVar) <- return $ find ((==ujobName) . jobName . fst) results
            uout <- atomically $ maybe retry return =<< readTVar uoutVar
            Just uart <- return $ find ((==uartName) . aoutName) $ outArtifacts uout
            return uart
        out <- runJob gitLock dir cid job uses
        atomically $ writeTVar outVar $ Just out
    return $ map snd results

runJob :: MVar () -> FilePath -> String -> Job -> [ArtifactOutput] -> IO JobOutput
runJob gitLock dir cid job uses = do
    [checkoutPath] <- lines <$> readProcess "mktemp" ["-d", "-t", "minici.XXXXXXXXXX"] ""

    tid <- withMVar gitLock $ \_ -> do
        "" <- readProcess "git" ["--work-tree=" <> checkoutPath, "restore", "--source=" <> cid, "--", "."] ""
        ["tree", tid]:_ <- map words . lines <$> readProcess "git" ["cat-file", "-p", cid] ""
        return tid

    let jdir = dir </> "jobs" </> tid </> stringJobName (jobName job)
    createDirectoryIfMissing True jdir
    logs <- openFile (jdir </> "log") WriteMode

    forM_ uses $ \aout -> do
        let target = checkoutPath </> aoutWorkPath aout
        createDirectoryIfMissing True $ takeDirectory target
        copyFile (aoutStorePath aout) target

    res <- runExceptT $ do
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
                throwError ()

    hClose logs

    writeFile (jdir </> "status") $
        if res == Right () then "success\n" else "failure\n"

    let adir = jdir </> "artifacts"
    artifacts <- forM (jobArtifacts job) $ \(name@(ArtifactName tname), pathCmd) -> do
        [path] <- lines <$> readCreateProcess pathCmd { cwd = Just checkoutPath } ""
        let target = adir </> T.unpack tname
        createDirectoryIfMissing True adir
        copyFile (checkoutPath </> path) target
        return $ ArtifactOutput
            { aoutName = name
            , aoutWorkPath = path
            , aoutStorePath = target
            }

    removeDirectoryRecursive checkoutPath

    return JobOutput
        { outName = jobName job
        , outStatus = res == Right ()
        , outArtifacts = artifacts
        }
