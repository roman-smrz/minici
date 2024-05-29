module Main (main) where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Process

import Config
import Job
import Version

data CmdlineOptions = CmdlineOptions
    { optShowHelp :: Bool
    , optShowVersion :: Bool
    }

defaultCmdlineOptions :: CmdlineOptions
defaultCmdlineOptions = CmdlineOptions
    { optShowHelp = False
    , optShowVersion = False
    }

options :: [OptDescr (CmdlineOptions -> CmdlineOptions)]
options =
    [ Option ['h'] ["help"]
        (NoArg $ \opts -> opts { optShowHelp = True })
        "show this help and exit"
    , Option ['V'] ["version"]
        (NoArg $ \opts -> opts { optShowVersion = True })
        "show version and exit"
    ]

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

displayStatusLine :: Text -> Text -> [TVar (JobStatus JobOutput)] -> IO ()
displayStatusLine prefix1 prefix2 statuses = do
    blinkVar <- newTVarIO False
    t <- forkIO $ forever $ do
        threadDelay 500000
        atomically $ writeTVar blinkVar . not =<< readTVar blinkVar
    go blinkVar ""
    killThread t
  where
    go blinkVar prev = do
        (ss, cur) <- atomically $ do
            ss <- mapM readTVar statuses
            blink <- readTVar blinkVar
            let cur = T.concat $ map ((" " <>) . showStatus blink) ss
            when (cur == prev) retry
            return (ss, cur)
        when (not $ T.null prev) $ putStr "\r"
        let prefix1' = if any jobStatusFailed ss then "\ESC[91m" <> prefix1 <> "\ESC[0m"
                                                 else prefix1
        T.putStr $ prefix1' <> prefix2 <> cur
        hFlush stdout

        if all jobStatusFinished ss
           then T.putStrLn ""
           else go blinkVar cur

main :: IO ()
main = do
    args <- getArgs
    opts <- case getOpt Permute options args of
        (o, _, []) -> return (foldl (flip id) defaultCmdlineOptions o)
        (_, _, errs) -> do
            hPutStrLn stderr $ concat errs <> "Try `minici --help' for more information."
            exitFailure

    when (optShowHelp opts) $ do
        let header = "Usage: minici [<option>...]"
        putStr $ usageInfo header options
        exitSuccess

    when (optShowVersion opts) $ do
        putStrLn versionLine
        exitSuccess

    Just configPath <- findConfig
    config <- parseConfig configPath

    commits <- map (fmap (drop 1) . (span (/=' '))) . lines <$>
        readProcess "git" ["log", "--pretty=oneline", "--first-parent", "--reverse", "origin/master..HEAD"] ""

    putStr $ replicate (8 + 50) ' '
    forM_ (configJobs config) $ \job -> do
        T.putStr $ (" "<>) $ fitToLength 7 $ textJobName $ jobName job
    putStrLn ""

    forM_ commits $ \(cid, desc) -> do
        let shortCid = T.pack $ take 7 cid
        outs <- runJobs "./.minici" cid $ configJobs config
        displayStatusLine shortCid (" " <> fitToLength 50 (T.pack desc)) outs
