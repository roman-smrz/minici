module Destination (
    Destination,
    DeclaredDestination(..),
    DestinationName(..),

    openDestination,
) where

import Data.Text (Text)
import Data.Text qualified as T

import System.Directory


data Destination
    = FilesystemDestination FilePath

data DeclaredDestination = DeclaredDestination
    { destinationName :: DestinationName
    , destinationUrl :: Maybe Text
    }


newtype DestinationName = DestinationName Text
    deriving (Eq, Ord, Show)


openDestination :: Text -> IO Destination
openDestination url = do
    let path = T.unpack url
    createDirectoryIfMissing True path
    return $ FilesystemDestination path
