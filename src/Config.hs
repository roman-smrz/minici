module Config (
    Config(..),
    findConfig,
    parseConfig,

    loadConfigForCommit,
    loadJobSetForCommit,
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
import System.FilePath
import System.Process

import Job.Types
import Repo


configFileName :: FilePath
configFileName = "minici.yaml"


data Config = Config
    { configJobs :: [ Job ]
    , configRepos :: [ DeclaredRepo ]
    }

instance Semigroup Config where
    a <> b = Config
        { configJobs = configJobs a ++ configJobs b
        , configRepos = configRepos a ++ configRepos b
        }

instance Monoid Config where
    mempty = Config
        { configJobs = []
        , configRepos = []
        }

instance FromYAML Config where
    parseYAML = withMap "Config" $ \m -> do
        let getpos = \case (Scalar pos _, _) -> pos
                           (Mapping pos _ _, _) -> pos
                           (Sequence pos _ _, _) -> pos
                           (Anchor pos _ _, _) -> pos
        foldM go mempty $ sortBy (comparing $ posLine . getpos) $ M.assocs m
      where
        go config = \case
            (Scalar _ (SStr tag), node)
                | [ "job", name ] <- T.words tag -> do
                    job <- parseJob name node
                    return $ config { configJobs = configJobs config ++ [ job ] }
                | [ "repo", name ] <- T.words tag -> do
                    repo <- parseRepo name node
                    return $ config { configRepos = configRepos config ++ [ repo ] }
            _ -> return config

parseJob :: Text -> Node Pos -> Parser Job
parseJob name node = flip (withMap "Job") node $ \j -> do
    let jobName = JobName name
    jobCheckout <- choice
        [ parseSingleCheckout =<< j .: "checkout"
        , parseMultipleCheckouts =<< j .: "checkout"
        , withNull "no checkout" (return []) =<< j .: "checkout"
        , return [ ( Nothing, Nothing ) ]
        ]
    jobRecipe <- choice
        [ cabalJob =<< j .: "cabal"
        , shellJob =<< j .: "shell"
        ]
    jobArtifacts <- parseArtifacts j
    jobUses <- maybe (return []) parseUses =<< j .:? "uses"
    return Job {..}

parseSingleCheckout :: Node Pos -> Parser [ ( Maybe RepoName, Maybe FilePath ) ]
parseSingleCheckout = withMap "checkout definition" $ \m -> do
    name <- m .:? "repo"
    subtree <- m .:? "subtree"
    return [ ( RepoName <$> name, T.unpack <$> subtree ) ]

parseMultipleCheckouts :: Node Pos -> Parser [ ( Maybe RepoName, Maybe FilePath ) ]
parseMultipleCheckouts = withSeq "checkout definitions" $ fmap concat . mapM parseSingleCheckout

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


parseRepo :: Text -> Node Pos -> Parser DeclaredRepo
parseRepo name node = flip (withMap "Repo") node $ \r -> DeclaredRepo
    <$> pure (RepoName name)
    <*> (T.unpack <$> r .: "path")


findConfig :: IO (Maybe FilePath)
findConfig = go "."
  where
    go path = do
        doesFileExist (path </> configFileName) >>= \case
            True -> return $ Just $ path </> configFileName
            False -> doesDirectoryExist (path </> "..") >>= \case
                True -> do
                    parent <- canonicalizePath $ path </> ".."
                    if parent /= path then go parent
                                      else return Nothing
                False -> return Nothing

parseConfig :: BS.ByteString -> Either String Config
parseConfig contents = do
    case decode1 contents of
        Left (pos, err) -> do
            Left $ prettyPosWithSource pos contents err
        Right conf -> Right conf

loadConfigForCommit :: Commit -> IO (Either String Config)
loadConfigForCommit commit = do
    readCommittedFile commit configFileName >>= return . \case
        Just content -> either (\_ -> Left $ "failed to parse " <> configFileName) Right $ parseConfig content
        Nothing -> Left $ configFileName <> " not found"

loadJobSetForCommit :: Commit -> IO JobSet
loadJobSetForCommit commit = toJobSet <$> loadConfigForCommit commit
  where
    toJobSet configEither = JobSet
        { jobsetCommit = commit
        , jobsetJobsEither = fmap configJobs configEither
        }
