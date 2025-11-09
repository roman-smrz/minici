module Destination (
    Destination,
    DeclaredDestination(..),
    DestinationName(..), textDestinationName, showDestinationName,

    openDestination,
    copyToDestination,

    copyRecursive,
    copyRecursiveForce,
) where

import Control.Monad
import Control.Monad.IO.Class

import Data.Text (Text)
import Data.Text qualified as T

import System.FilePath
import System.Directory


data Destination
    = FilesystemDestination FilePath

data DeclaredDestination = DeclaredDestination
    { destinationName :: DestinationName
    , destinationUrl :: Maybe Text
    }


newtype DestinationName = DestinationName Text
    deriving (Eq, Ord, Show)

textDestinationName :: DestinationName -> Text
textDestinationName (DestinationName text) = text

showDestinationName :: DestinationName -> String
showDestinationName = T.unpack . textDestinationName


openDestination :: FilePath -> Text -> IO Destination
openDestination baseDir url = do
    let path = baseDir </> T.unpack url
    createDirectoryIfMissing True path
    return $ FilesystemDestination path

copyToDestination :: MonadIO m => FilePath -> Destination -> FilePath -> m ()
copyToDestination source (FilesystemDestination base) inner = do
    let target = base </> dropWhile isPathSeparator inner
    liftIO $ do
        createDirectoryIfMissing True $ takeDirectory target
        copyRecursiveForce source target


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
