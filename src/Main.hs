module Main (main) where

import Control.Monad
import Control.Monad.Except

import Data.Text (Text)
import Data.Text qualified as T

import System.Directory
import System.Exit
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

data ArtifactName = ArtifactName Text
    deriving (Eq, Ord, Show)


runJob :: String -> Job -> IO JobOutput
runJob cid job = do
    [path] <- lines <$> readProcess "mktemp" ["-d", "-t", "minici.XXXXXXXXXX"] ""

    "" <- readProcess "git" ["--work-tree=" <> path, "restore", "--source=" <> cid, "--", "."] ""

    res <- runExceptT $ do
        forM_ (jobRecipe job) $ \p -> do
            (Just hin, Just hout, Just herr, hp) <- liftIO $ createProcess p
                { cwd = Just path
                , std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
            liftIO $ hClose hin
            exit <- liftIO $ waitForProcess hp
            liftIO $ hClose hout
            liftIO $ hClose herr

            when (exit /= ExitSuccess) $
                throwError ()

    removeDirectoryRecursive $ path

    return JobOutput
        { outName = jobName job
        , outStatus = res == Right ()
        , outArtifacts = []
        }


cabalJob :: JobName -> [ArtifactName] -> Job
cabalJob name artifacts = Job
    { jobName = name
    , jobRecipe = [ proc "cabal" ["build", "--ghc-option=-Werror"] ]
    , jobArtifacts = map (\(ArtifactName aname) -> (ArtifactName "bin", proc "cabal" ["list-bin", T.unpack aname])) artifacts
    }

main :: IO ()
main = do
    commits <- map (fmap (drop 1) . (span (/=' '))) . lines <$>
        readProcess "git" ["log", "--pretty=oneline", "--first-parent", "--reverse", "origin/master..HEAD"] ""
    forM_ commits $ \(cid, desc) -> do
        let descLen = length desc
            desc' = if descLen <= 50 then desc <> replicate (50 - descLen) ' '
                                     else take 49 desc <> "…"
            shortCid = take 7 cid

        putStr $ shortCid <> " " <> desc' <> " "
        hFlush stdout
        runJob cid (cabalJob (JobName "build") $ map (ArtifactName . T.pack) []) >>= \case
            out | outStatus out -> do
                putStr "\ESC[92m✓\ESC[0m"
            _ -> do
                putStr "\ESC[91m✗\ESC[0m"
                putStr $ "\r\ESC[91m" <> shortCid <> "\ESC[0m"
        putStrLn ""
