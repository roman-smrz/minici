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
import Data.Text (Text)
import Data.Text qualified as T

import Config
import Job.Types
import Repo

data EvalInput = EvalInput
    { eiContainingRepo :: Maybe Repo
    , eiOtherRepos :: [ ( RepoName, Repo ) ]
    }

data EvalError
    = OtherEvalError Text

textEvalError :: EvalError -> Text
textEvalError (OtherEvalError text) = text


type Eval a = ReaderT EvalInput (ExceptT EvalError IO) a

runEval :: Eval a -> EvalInput -> IO (Either EvalError a)
runEval action einput = runExceptT $ flip runReaderT einput action


evalJob :: EvalInput -> DeclaredJob -> Except EvalError Job
evalJob EvalInput {..} decl = do
    otherCheckout <- forM (jobOtherCheckout decl) $ \( DeclaredJobRepo name, revision, checkout ) -> do
        repo <- maybe (throwError $ OtherEvalError $ "repo `" <> textRepoName name <> "' not defined") return $
            lookup name eiOtherRepos
        return ( EvaluatedJobRepo repo, revision, checkout )
    return Job
        { jobName = jobName decl
        , jobContainingCheckout = jobContainingCheckout decl
        , jobOtherCheckout = otherCheckout
        , jobRecipe = jobRecipe decl
        , jobArtifacts = jobArtifacts decl
        , jobUses = jobUses decl
        }

evalJobSet :: EvalInput -> DeclaredJobSet -> JobSet
evalJobSet ei decl = do
    JobSet
        { jobsetCommit = jobsetCommit decl
        , jobsetJobsEither = join $
            fmap (sequence . map (runExceptStr . evalJob ei)) $
            jobsetJobsEither decl
        }
  where
    runExceptStr = first (T.unpack . textEvalError) . runExcept


canonicalJobName :: [ Text ] -> Config -> Eval [ JobIdPart ]
canonicalJobName (r : rs) config = do
    einput <- ask
    let name = JobName r
    case find ((name ==) . jobName) (configJobs config) of
        Just djob -> do
            job <- either throwError return $ runExcept $ evalJob einput djob
            let repos = nub $ map (\( EvaluatedJobRepo repo, _, _ ) -> repo) $ jobOtherCheckout job
            (JobIdName name :) <$> canonicalOtherCheckouts rs repos
        Nothing -> throwError $ OtherEvalError $ "job ‘" <> r <> "’ not found"
canonicalJobName [] _ = throwError $ OtherEvalError "expected job name"

canonicalOtherCheckouts :: [ Text ] -> [ Repo ] -> Eval [ JobIdPart ]
canonicalOtherCheckouts (r : rs) (repo : repos) = do
    tree <- tryReadCommit repo r >>= \case
        Just commit -> getCommitTree commit
        Nothing -> tryReadTree repo r >>= \case
            Just tree -> return tree
            Nothing -> throwError $ OtherEvalError $ "failed to resolve ‘" <> r <> "’ to a commit or tree in " <> T.pack (show repo)
    (JobIdTree (treeId tree) :) <$> canonicalOtherCheckouts rs repos
canonicalOtherCheckouts []       []       = return []
canonicalOtherCheckouts []       (_ : _ ) = throwError $ OtherEvalError $ "expected commit or tree reference"
canonicalOtherCheckouts (r : _)  []       = throwError $ OtherEvalError $ "unexpected job ref part ‘" <> r <> "’"

canonicalCommitConfig :: [ Text ] -> Repo -> Eval [ JobIdPart ]
canonicalCommitConfig (r : rs) repo = do
    tree <- tryReadCommit repo r >>= \case
        Just commit -> getCommitTree commit
        Nothing -> tryReadTree repo r >>= \case
            Just tree -> return tree
            Nothing -> throwError $ OtherEvalError $ "failed to resolve ‘" <> r <> "’ to a commit or tree in " <> T.pack (show repo)
    config <- either fail return =<< loadConfigForCommit tree
    (JobIdTree (treeId tree) :) <$> canonicalJobName rs config
canonicalCommitConfig [] _ = throwError $ OtherEvalError "expected commit or tree reference"

evalJobReference :: Config -> JobRef -> Eval JobId
evalJobReference config (JobRef rs) =
    fmap JobId $ do
        asks eiContainingRepo >>= \case
            Just defRepo -> do
                canonicalCommitConfig rs defRepo
            Nothing -> do
                canonicalJobName rs config
