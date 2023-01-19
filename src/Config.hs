module Config (
    Config(..),
    findConfig,
    parseConfig,
) where

import Control.Monad
import Control.Monad.Combinators

import Data.ByteString.Lazy qualified as BS
import Data.List
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as T
import Data.YAML

import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Job

data Config = Config
    { configJobs :: [Job]
    }

instance Semigroup Config where
    a <> b = Config
        { configJobs = configJobs a ++ configJobs b
        }

instance Monoid Config where
    mempty = Config
        { configJobs = []
        }

instance FromYAML Config where
    parseYAML = withMap "Config" $ \m -> do
        let getpos = \case (Scalar pos _, _) -> pos
                           (Mapping pos _ _, _) -> pos
                           (Sequence pos _ _, _) -> pos
                           (Anchor pos _ _, _) -> pos
        jobs <- fmap catMaybes $ forM (sortBy (comparing $ posLine . getpos) $ M.assocs m) $ \case
            (Scalar _ (SStr tag), node) | ["job", name] <- T.words tag -> do
                Just <$> parseJob name node
            _ -> return Nothing
        return $ Config jobs

parseJob :: Text -> Node Pos -> Parser Job
parseJob name node = flip (withMap "Job") node $ \j -> Job
    <$> pure (JobName name)
    <*> choice
        [ cabalJob =<< j .: "cabal"
        , shellJob =<< j .: "shell"
        ]
    <*> parseArtifacts j
    <*> (maybe (return []) parseUses =<< j .:? "uses")

cabalJob :: Node Pos -> Parser [CreateProcess]
cabalJob = withMap "cabal job" $ \m -> do
    ghcOptions <- m .:? "ghc-options" >>= \case
        Nothing -> return []
        Just s -> withSeq "GHC option list" (mapM (withStr "GHC option" return)) s

    return
        [ proc "cabal" $ concat [ ["build"], ("--ghc-option="++) . T.unpack <$> ghcOptions ] ]

shellJob :: Node Pos -> Parser [CreateProcess]
shellJob = withSeq "shell commands" $ \xs -> do
    fmap (map shell) $ forM xs $ withStr "shell command" $ return . T.unpack

parseArtifacts :: Mapping Pos -> Parser [(ArtifactName, CreateProcess)]
parseArtifacts m = do
    fmap catMaybes $ forM (M.assocs m) $ \case
        (Scalar _ (SStr tag), node) | ["artifact", name] <- T.words tag -> do
            Just <$> parseArtifact name node
        _ -> return Nothing
  where
    parseArtifact name = withMap "Artifact" $ \am -> do
        path <- am .: "path"
        return (ArtifactName name, proc "echo" [ T.unpack path ])

parseUses :: Node Pos -> Parser [(JobName, ArtifactName)]
parseUses = withSeq "Uses list" $ mapM $
    withStr "Artifact reference" $ \text -> do
        [job, art] <- return $ T.split (== '.') text
        return (JobName job, ArtifactName art)

findConfig :: IO (Maybe FilePath)
findConfig = go "."
  where
    name = "minici.yaml"
    go path = do
        doesFileExist (path </> name) >>= \case
            True -> return $ Just $ path </> name
            False -> doesDirectoryExist (path </> "..") >>= \case
                True -> do
                    parent <- canonicalizePath $ path </> ".."
                    if parent /= path then go parent
                                      else return Nothing
                False -> return Nothing

parseConfig :: FilePath -> IO Config
parseConfig path = do
    contents <- BS.readFile path
    case decode1 contents of
        Left (pos, err) -> do
            putStr $ prettyPosWithSource pos contents err
            exitFailure
        Right conf -> return conf
