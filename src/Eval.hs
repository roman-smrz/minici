module Eval (
    EvalInput(..),
    EvalError(..), textEvalError,
    Eval, runEval,

    evalJob,
    evalJobSet,
    evalJobReference,
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

import System.FilePath

import Config
import Job.Types
import Repo

data EvalInput = EvalInput
    { eiJobRoot :: JobRoot
    , eiRootPath :: FilePath
    , eiCurrentIdRev :: [ JobIdPart ]
    , eiContainingRepo :: Maybe Repo
    , eiOtherRepos :: [ ( RepoName, Repo ) ]
    }

data EvalError
    = OtherEvalError Text

textEvalError :: EvalError -> Text
textEvalError (OtherEvalError text) = text


type Eval a = ReaderT EvalInput (ExceptT EvalError IO) a

runEval :: Eval a -> EvalInput -> IO (Either EvalError a)
runEval action einput = runExceptT $ flip runReaderT einput action


commonPrefix :: Eq a => [ a ] -> [ a ] -> [ a ]
commonPrefix (x : xs) (y : ys) | x == y = x : commonPrefix xs ys
commonPrefix _        _                 = []

isDefaultRepoMissingInId :: DeclaredJob -> Eval Bool
isDefaultRepoMissingInId djob
    | all (isJust . jcRepo) (jobCheckout djob) = return False
    | otherwise = asks (not . any matches . eiCurrentIdRev)
  where
    matches (JobIdName _) = False
    matches (JobIdCommit rname _) = isNothing rname
    matches (JobIdTree rname _ _) = isNothing rname

collectOtherRepos :: DeclaredJobSet -> DeclaredJob -> Eval [ ( Maybe ( RepoName, Maybe Text ), FilePath ) ]
collectOtherRepos dset decl = do
    let dependencies = map fst $ jobUses decl
    dependencyRepos <- forM dependencies $ \name -> do
        jobs <- either (throwError . OtherEvalError . T.pack) return $ jobsetJobsEither dset
        job <- maybe (throwError $ OtherEvalError $ "job ‘" <> textJobName name <> "’ not found") return . find ((name ==) . jobName) $ jobs
        return $ jobCheckout job

    missingDefault <- isDefaultRepoMissingInId decl

    let checkouts =
            (if missingDefault then id else (filter (isJust . jcRepo))) $
            concat
                [ jobCheckout decl
                , concat dependencyRepos
                ]
    let commonSubdir reporev = joinPath $ foldr1 commonPrefix $
            map (maybe [] splitDirectories . jcSubtree) . filter ((reporev ==) . jcRepo) $ checkouts
    return $ map (\r -> ( r, commonSubdir r )) . nub . map jcRepo $ checkouts


evalJob :: [ ( Maybe RepoName, Tree ) ] -> DeclaredJobSet -> DeclaredJob -> Eval Job
evalJob revisionOverrides dset decl = do
    EvalInput {..} <- ask
    otherRepos <- collectOtherRepos dset decl
    otherRepoTrees <- forM otherRepos $ \( mbrepo, commonPath ) -> do
        ( mbrepo, ) . ( commonPath, ) <$> do
            case lookup (fst <$> mbrepo) revisionOverrides of
                Just tree -> return tree
                Nothing -> do
                    repo <- evalRepo (fst <$> mbrepo)
                    commit <- readCommit repo (fromMaybe "HEAD" $ join $ snd <$> mbrepo)
                    getSubtree (Just commit) commonPath =<< getCommitTree commit

    checkouts <- forM (jobCheckout decl) $ \dcheckout -> do
        return dcheckout
            { jcRepo =
                fromMaybe (error $ "expecting repo in either otherRepoTrees or revisionOverrides: " <> show (textRepoName . fst <$> jcRepo dcheckout)) $
                msum
                    [ snd <$> lookup (jcRepo dcheckout) otherRepoTrees
                    , lookup (fst <$> jcRepo dcheckout) revisionOverrides
                    ]
            }

    let otherRepoIds = map (\( repo, ( subtree, tree )) -> JobIdTree (fst <$> repo) subtree (treeId tree)) otherRepoTrees
    return Job
        { jobId = JobId $ reverse $ reverse otherRepoIds ++ JobIdName (jobId decl) : eiCurrentIdRev
        , jobName = jobName decl
        , jobCheckout = checkouts
        , jobRecipe = jobRecipe decl
        , jobArtifacts = jobArtifacts decl
        , jobUses = jobUses decl
        }

evalJobSet :: [ ( Maybe RepoName, Tree ) ] -> DeclaredJobSet -> Eval JobSet
evalJobSet revisionOverrides decl = do
    jobs <- either (return . Left) (handleToEither . mapM (evalJob revisionOverrides decl)) $ jobsetJobsEither decl
    return JobSet
        { jobsetCommit = jobsetCommit decl
        , jobsetJobsEither = jobs
        }
  where
    handleToEither = handleError (return . Left . T.unpack . textEvalError) . fmap Right

evalRepo :: Maybe RepoName -> Eval Repo
evalRepo Nothing = asks eiContainingRepo >>= \case
    Just repo -> return repo
    Nothing -> throwError $ OtherEvalError $ "no default repo"
evalRepo (Just name) = asks (lookup name . eiOtherRepos) >>= \case
    Just repo -> return repo
    Nothing -> throwError $ OtherEvalError $ "repo ‘" <> textRepoName name <> "’ not defined"


canonicalJobName :: [ Text ] -> Config -> Maybe Tree -> Eval Job
canonicalJobName (r : rs) config mbDefaultRepo = do
    let name = JobName r
        dset = JobSet Nothing $ Right $ configJobs config
    case find ((name ==) . jobName) (configJobs config) of
        Just djob -> do
            otherRepos <- collectOtherRepos dset djob
            ( overrides, rs' ) <- (\f -> foldM f ( [], rs ) otherRepos) $
                \( overrides, crs ) ( mbrepo, path ) -> do
                    ( tree, crs' ) <- readTreeFromIdRef crs path =<< evalRepo (fst <$> mbrepo)
                    return ( ( fst <$> mbrepo, tree ) : overrides, crs' )
            case rs' of
                (r' : _) -> throwError $ OtherEvalError $ "unexpected job ref part ‘" <> r' <> "’"
                _ -> return ()
            evalJob (maybe id ((:) . ( Nothing, )) mbDefaultRepo $ overrides) dset djob
        Nothing -> throwError $ OtherEvalError $ "job ‘" <> r <> "’ not found"
canonicalJobName [] _ _ = throwError $ OtherEvalError "expected job name"

readTreeFromIdRef :: [ Text ] -> FilePath -> Repo -> Eval ( Tree, [ Text ] )
readTreeFromIdRef (r : rs) subdir repo = do
    tryReadCommit repo r >>= \case
        Just commit -> return . (, rs) =<< getSubtree (Just commit) subdir =<< getCommitTree commit
        Nothing -> tryReadTree repo subdir r >>= \case
            Just tree -> return ( tree, rs )
            Nothing -> throwError $ OtherEvalError $ "failed to resolve ‘" <> r <> "’ to a commit or tree in " <> T.pack (show repo)
readTreeFromIdRef [] _ _ = throwError $ OtherEvalError $ "expected commit or tree reference"

canonicalCommitConfig :: [ Text ] -> Repo -> Eval Job
canonicalCommitConfig rs repo = do
    ( tree, rs' ) <- readTreeFromIdRef rs "" repo
    config <- either fail return =<< loadConfigForCommit tree
    local (\ei -> ei { eiCurrentIdRev = JobIdTree Nothing "" (treeId tree) : eiCurrentIdRev ei }) $
        canonicalJobName rs' config (Just tree)

evalJobReference :: JobRef -> Eval Job
evalJobReference (JobRef rs) =
    asks eiJobRoot >>= \case
        JobRootRepo defRepo -> do
            canonicalCommitConfig rs defRepo
        JobRootConfig config -> do
            canonicalJobName rs config Nothing
