module Job (
    Job(..),
    JobOutput(..),
    JobName(..), stringJobName,
    ArtifactName(..),
    runJob,
) where

import Control.Monad
import Control.Monad.Except

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
    }

data JobOutput = JobOutput
    { outName :: JobName
    , outStatus :: Bool
    , outArtifacts :: [(ArtifactName, FilePath)]
    }
    deriving (Show)

data JobName = JobName Text
    deriving (Eq, Ord, Show)

stringJobName :: JobName -> String
stringJobName (JobName name) = T.unpack name

data ArtifactName = ArtifactName Text
    deriving (Eq, Ord, Show)


runJob :: FilePath -> String -> Job -> IO JobOutput
runJob dir cid job = do
    [path] <- lines <$> readProcess "mktemp" ["-d", "-t", "minici.XXXXXXXXXX"] ""

    "" <- readProcess "git" ["--work-tree=" <> path, "restore", "--source=" <> cid, "--", "."] ""
    ["tree", tid]:_  <- map words . lines <$> readProcess "git" ["cat-file", "-p", cid] ""

    let jdir = dir </> "jobs" </> tid </> stringJobName (jobName job)
    createDirectoryIfMissing True jdir
    logs <- openFile (jdir </> "log") WriteMode

    res <- runExceptT $ do
        forM_ (jobRecipe job) $ \p -> do
            (Just hin, _, _, hp) <- liftIO $ createProcess_ "" p
                { cwd = Just path
                , std_in = CreatePipe
                , std_out = UseHandle logs
                , std_err = UseHandle logs
                }
            liftIO $ hClose hin
            exit <- liftIO $ waitForProcess hp

            when (exit /= ExitSuccess) $
                throwError ()

    hClose logs
    removeDirectoryRecursive $ path

    writeFile (jdir </> "status") $
        if res == Right () then "success\n" else "failure\n"

    return JobOutput
        { outName = jobName job
        , outStatus = res == Right ()
        , outArtifacts = []
        }
