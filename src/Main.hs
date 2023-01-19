module Main (main) where

import Control.Concurrent.STM

import Control.Monad

import System.IO
import System.Process

import Config
import Job

fitToLength :: Int -> String -> String
fitToLength maxlen str | len <= maxlen = str <> replicate (maxlen - len) ' '
                       | otherwise     = take (maxlen - 1) str <> "…"
    where len = length str

main :: IO ()
main = do
    Just configPath <- findConfig
    config <- parseConfig configPath

    commits <- map (fmap (drop 1) . (span (/=' '))) . lines <$>
        readProcess "git" ["log", "--pretty=oneline", "--first-parent", "--reverse", "origin/master..HEAD"] ""

    putStr $ replicate (8 + 50) ' '
    forM_ (configJobs config) $ \job -> do
        putStr $ (' ':) $ fitToLength 7 $ stringJobName $ jobName job
    putStrLn ""

    forM_ commits $ \(cid, desc) -> do
        let shortCid = take 7 cid
        putStr $ shortCid <> " " <> fitToLength 50 desc
        hFlush stdout
        outs <- runJobs "./.minici" cid $ configJobs config
        results <- forM outs $ \outVar -> do
            putStr " "
            hFlush stdout
            out <- atomically $ maybe retry return =<< readTVar outVar
            if | outStatus out -> do
                    putStr "\ESC[92m✓\ESC[0m      "
               | otherwise -> do
                    putStr "\ESC[91m✗\ESC[0m      "
            hFlush stdout
            return $ outStatus out

        when (not $ and results) $ do
            putStr $ "\r\ESC[91m" <> shortCid <> "\ESC[0m"
        putStrLn ""
