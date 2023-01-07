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
                flip (withMap "Job") node $ \j -> Just <$> choice
                    [ cabalJob name =<< j .: "cabal"
                    , shellJob name =<< j .: "shell"
                    ]
            _ -> return Nothing
        return $ Config jobs

cabalJob :: Text -> Node Pos -> Parser Job
cabalJob name = withMap "cabal job" $ \m -> do
    ghcOptions <- m .:? "ghc-options" >>= \case
        Nothing -> return []
        Just s -> withSeq "GHC option list" (mapM (withStr "GHC option" return)) s

    return Job
        { jobName = JobName name
        , jobRecipe = [ proc "cabal" $ concat [ ["build"], ("--ghc-option="++) . T.unpack <$> ghcOptions ] ]
        , jobArtifacts = map (\(ArtifactName aname) -> (ArtifactName "bin", proc "cabal" ["list-bin", T.unpack aname])) []
        }

shellJob :: Text -> Node Pos -> Parser Job
shellJob name = withSeq "shell commands" $ \xs -> do
    recipe <- forM xs $ withStr "shell command" $ return . T.unpack
    return Job
        { jobName = JobName name
        , jobRecipe = map shell recipe
        , jobArtifacts = []
        }

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
