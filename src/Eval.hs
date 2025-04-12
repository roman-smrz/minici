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

import Data.Bifunctor
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
    | [] <- jobContainingCheckout djob = return False
    | otherwise = asks (not . any matches . eiCurrentIdRev)
  where
    matches (JobIdName _) = False
    matches (JobIdCommit rname _) = isNothing rname
    matches (JobIdTree rname _) = isNothing rname

collectOtherRepos :: DeclaredJob -> Eval [ (( Maybe RepoName, Maybe Text ), FilePath ) ]
collectOtherRepos decl = do
    missingDefault <- isDefaultRepoMissingInId decl
    let checkouts = concat
            [ if missingDefault then map (( Nothing, Nothing ), ) $ jobContainingCheckout decl else []
            , map (first (first Just)) $ jobOtherCheckout decl
            ]
    let commonSubdir reporev = joinPath $ foldr commonPrefix [] $
            map (maybe [] splitDirectories . jcSubtree . snd) . filter ((reporev ==) . fst) $ checkouts
    return $ map (\r -> ( r, commonSubdir r )) . nub . map fst $ checkouts


evalJob :: [ ( Maybe RepoName, Tree ) ] -> DeclaredJob -> Eval Job
evalJob revisionOverrides decl = do
    EvalInput {..} <- ask
    otherRepos <- collectOtherRepos decl
    otherRepoIds <- forM otherRepos $ \(( mbname, mbrev ), _ ) -> do
        tree <- case lookup mbname revisionOverrides of
            Just tree -> return tree
            Nothing -> case maybe eiContainingRepo (flip lookup eiOtherRepos) mbname of
                Just repo -> getCommitTree =<< readCommit repo (fromMaybe "HEAD" mbrev)
                Nothing -> throwError $ OtherEvalError $ "repo ‘" <> maybe "" textRepoName mbname <> "’ not defined"
        return $ JobIdTree mbname $ treeId tree
    otherCheckout <- forM (jobOtherCheckout decl) $ \(( name, revision ), checkout ) -> do
        tree <- case lookup (Just name) revisionOverrides of
            Just tree -> return tree
            Nothing -> case lookup name eiOtherRepos of
                Just repo -> getCommitTree =<< readCommit repo (fromMaybe "HEAD" revision)
                Nothing -> throwError $ OtherEvalError $ "repo ‘" <> textRepoName name <> "’ not defined"
        return ( tree, checkout )
    return Job
        { jobId = JobId $ reverse $ reverse otherRepoIds ++ JobIdName (jobId decl) : eiCurrentIdRev
        , jobName = jobName decl
        , jobContainingCheckout = jobContainingCheckout decl
        , jobOtherCheckout = otherCheckout
        , jobRecipe = jobRecipe decl
        , jobArtifacts = jobArtifacts decl
        , jobUses = jobUses decl
        }

evalJobSet :: [ ( Maybe RepoName, Tree ) ] -> DeclaredJobSet -> Eval JobSet
evalJobSet revisionOverrides decl = do
    jobs <- either (return . Left) (handleToEither . mapM (evalJob revisionOverrides)) $ jobsetJobsEither decl
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


canonicalJobName :: [ Text ] -> Config -> Eval JobId
canonicalJobName (r : rs) config = do
    let name = JobName r
    case find ((name ==) . jobName) (configJobs config) of
        Just djob -> do
            otherRepos <- collectOtherRepos djob
            ( overrides, rs' ) <- (\f -> foldM f ( [], rs ) otherRepos) $
                \( overrides, crs ) (( mbname, _ ), _ ) -> do
                    ( tree, crs' ) <- readTreeFromIdRef crs =<< evalRepo mbname
                    return ( ( mbname, tree ) : overrides, crs' )
            case rs' of
                (r' : _) -> throwError $ OtherEvalError $ "unexpected job ref part ‘" <> r' <> "’"
                _ -> return ()
            jobId <$> evalJob overrides djob
        Nothing -> throwError $ OtherEvalError $ "job ‘" <> r <> "’ not found"
canonicalJobName [] _ = throwError $ OtherEvalError "expected job name"

readTreeFromIdRef :: [ Text ] -> Repo -> Eval ( Tree, [ Text ] )
readTreeFromIdRef (r : rs) repo = do
    tryReadCommit repo r >>= \case
        Just commit -> (, rs) <$> getCommitTree commit
        Nothing -> tryReadTree repo r >>= \case
            Just tree -> return ( tree, rs )
            Nothing -> throwError $ OtherEvalError $ "failed to resolve ‘" <> r <> "’ to a commit or tree in " <> T.pack (show repo)
readTreeFromIdRef [] _ = throwError $ OtherEvalError $ "expected commit or tree reference"

canonicalCommitConfig :: [ Text ] -> Repo -> Eval JobId
canonicalCommitConfig rs repo = do
    ( tree, rs' ) <- readTreeFromIdRef rs repo
    config <- either fail return =<< loadConfigForCommit tree
    local (\ei -> ei { eiCurrentIdRev = JobIdTree Nothing (treeId tree) : eiCurrentIdRev ei }) $
        canonicalJobName rs' config

evalJobReference :: JobRef -> Eval JobId
evalJobReference (JobRef rs) =
    asks eiJobRoot >>= \case
        JobRootRepo defRepo -> do
            canonicalCommitConfig rs defRepo
        JobRootConfig config -> do
            canonicalJobName rs config
