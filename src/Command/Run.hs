module Command.Run (
    RunCommand,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader

import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.Exit
import System.IO
import System.Process

import Command
import Config
import Job

data RunCommand = RunCommand Text

instance Command RunCommand where
    commandName _ = "run"

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
    config <- getConfig
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

    liftIO $ do
        commits <- map (fmap (drop 1) . (span (/=' '))) . lines <$>
            readProcess "git" [ "log", "--pretty=oneline", "--first-parent", "--reverse", base <> ".." <> tip ] ""

        putStr $ replicate (8 + 50) ' '
        forM_ (configJobs config) $ \job -> do
            T.putStr $ (" "<>) $ fitToLength 7 $ textJobName $ jobName job
        putStrLn ""

        forM_ commits $ \(cid, desc) -> do
            let shortCid = T.pack $ take 7 cid
            outs <- runJobs "./.minici" cid $ configJobs config
            displayStatusLine shortCid (" " <> fitToLength 50 (T.pack desc)) outs


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
