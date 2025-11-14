module FileUtils where

import Control.Monad

import System.FilePath
import System.Directory


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
