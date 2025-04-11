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


evalJob :: DeclaredJob -> Eval Job
evalJob decl = do
    EvalInput {..} <- ask
    otherCheckout <- forM (jobOtherCheckout decl) $ \( name, revision, checkout ) -> do
        repo <- maybe (throwError $ OtherEvalError $ "repo `" <> textRepoName name <> "' not defined") return $
            lookup name eiOtherRepos
        return ( repo, revision, checkout )
    return Job
        { jobId = JobId $ reverse $ JobIdName (jobId decl) : eiCurrentIdRev
        , jobName = jobName decl
        , jobContainingCheckout = jobContainingCheckout decl
        , jobOtherCheckout = otherCheckout
        , jobRecipe = jobRecipe decl
        , jobArtifacts = jobArtifacts decl
        , jobUses = jobUses decl
        }

evalJobSet :: DeclaredJobSet -> Eval JobSet
evalJobSet decl = do
    jobs <- either (return . Left) (handleToEither . mapM evalJob) $ jobsetJobsEither decl
    return JobSet
        { jobsetCommit = jobsetCommit decl
        , jobsetJobsEither = jobs
        }
  where
    handleToEither = handleError (return . Left . T.unpack . textEvalError) . fmap Right


canonicalJobName :: [ Text ] -> Maybe Tree -> Config -> Eval [ JobIdPart ]
canonicalJobName (r : rs) mbTree config = do
    let name = JobName r
    case find ((name ==) . jobName) (configJobs config) of
        Just djob -> do
            job <- evalJob djob
            repos <- concat <$> sequence
                [ case mbTree of
                    Just _ -> return []
                    Nothing -> maybeToList <$> asks eiContainingRepo
                , return $ nub $ map (\( repo, _, _ ) -> repo) $ jobOtherCheckout job
                ]
            (JobIdName name :) <$> canonicalOtherCheckouts rs repos
        Nothing -> throwError $ OtherEvalError $ "job ‘" <> r <> "’ not found"
canonicalJobName [] _ _ = throwError $ OtherEvalError "expected job name"

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
    (JobIdTree (treeId tree) :) <$> canonicalJobName rs (Just tree) config
canonicalCommitConfig [] _ = throwError $ OtherEvalError "expected commit or tree reference"

evalJobReference :: JobRef -> Eval JobId
evalJobReference (JobRef rs) =
    JobId <$> do
        asks eiJobRoot >>= \case
            JobRootRepo defRepo -> do
                canonicalCommitConfig rs defRepo
            JobRootConfig config -> do
                canonicalJobName rs Nothing config
