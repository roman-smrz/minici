module Config (
    JobRoot(..),
    Config(..),
    findConfig,
    parseConfig,

    loadConfigForCommit,
    loadJobSetForCommit,
) where

import Control.Monad
import Control.Monad.Combinators
import Control.Monad.IO.Class

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
import System.FilePath.Glob
import System.Process

import Job.Types
import Repo


configFileName :: FilePath
configFileName = "minici.yaml"


data JobRoot
    = JobRootRepo Repo
    | JobRootConfig Config


data Config = Config
    { configJobs :: [ DeclaredJob ]
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

parseJob :: Text -> Node Pos -> Parser DeclaredJob
parseJob name node = flip (withMap "Job") node $ \j -> do
    let jobName = JobName name
        jobId = jobName
    jobCheckout <- choice
        [ parseSingleCheckout =<< j .: "checkout"
        , parseMultipleCheckouts =<< j .: "checkout"
        , withNull "no checkout" (return []) =<< j .: "checkout"
        , return [ JobCheckout Nothing Nothing Nothing ]
        ]
    jobRecipe <- choice
        [ cabalJob =<< j .: "cabal"
        , shellJob =<< j .: "shell"
        ]
    jobArtifacts <- parseArtifacts j
    jobUses <- maybe (return []) parseUses =<< j .:? "uses"
    return Job {..}

parseSingleCheckout :: Node Pos -> Parser [ JobCheckout Declared ]
parseSingleCheckout = withMap "checkout definition" $ \m -> do
    jcSubtree <- fmap T.unpack <$> m .:? "subtree"
    jcDestination <- fmap T.unpack <$> m .:? "dest"
    jcRepo <- m .:? "repo" >>= \case
        Nothing -> return Nothing
        Just name -> do
            revision <- m .:? "revision"
            return $ Just ( RepoName name, revision )
    return [ JobCheckout {..} ]

parseMultipleCheckouts :: Node Pos -> Parser [ JobCheckout Declared ]
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

parseArtifacts :: Mapping Pos -> Parser [ ( ArtifactName, Pattern ) ]
parseArtifacts m = do
    fmap catMaybes $ forM (M.assocs m) $ \case
        (Scalar _ (SStr tag), node) | ["artifact", name] <- T.words tag -> do
            Just <$> parseArtifact name node
        _ -> return Nothing
  where
    parseArtifact name = withMap "Artifact" $ \am -> do
        pat <- compile . T.unpack <$> am .: "path"
        return ( ArtifactName name, pat )

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

loadConfigForCommit :: MonadIO m => Tree -> m (Either String Config)
loadConfigForCommit tree = do
    readCommittedFile tree configFileName >>= return . \case
        Just content -> either (\_ -> Left $ "failed to parse " <> configFileName) Right $ parseConfig content
        Nothing -> Left $ configFileName <> " not found"

loadJobSetForCommit :: (MonadIO m, MonadFail m) => Commit -> m DeclaredJobSet
loadJobSetForCommit commit = return . toJobSet =<< loadConfigForCommit =<< getCommitTree commit
  where
    toJobSet configEither = JobSet
        { jobsetId = ()
        , jobsetCommit = Just commit
        , jobsetJobsEither = fmap configJobs configEither
        }
