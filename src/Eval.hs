module Eval (
    EvalInput(..),
    EvalError(..), textEvalError,

    evalJob,
    evalJobSet,
) where

import Control.Monad
import Control.Monad.Except

import Data.Bifunctor
import Data.Text (Text)
import Data.Text qualified as T

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
