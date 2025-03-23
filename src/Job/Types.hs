module Job.Types where

import Data.Text (Text)
import Data.Text qualified as T

import System.FilePath.Glob
import System.Process

import Repo


data Declared
data Evaluated

data Job' d = Job
    { jobName :: JobName
    , jobContainingCheckout :: [ JobCheckout ]
    , jobOtherCheckout :: [ ( JobRepo d, Maybe Text, JobCheckout ) ]
    , jobRecipe :: [ CreateProcess ]
    , jobArtifacts :: [ ( ArtifactName, Pattern ) ]
    , jobUses :: [ ( JobName, ArtifactName ) ]
    }

type Job = Job' Evaluated
type DeclaredJob = Job' Declared

data JobName = JobName Text
    deriving (Eq, Ord, Show)

stringJobName :: JobName -> String
stringJobName (JobName name) = T.unpack name

textJobName :: JobName -> Text
textJobName (JobName name) = name


data JobRepo d where
    DeclaredJobRepo :: RepoName -> JobRepo Declared
    EvaluatedJobRepo :: Repo -> JobRepo Evaluated

data JobCheckout = JobCheckout
    { jcSubtree :: Maybe FilePath
    , jcDestination :: Maybe FilePath
    }


data ArtifactName = ArtifactName Text
    deriving (Eq, Ord, Show)


data JobSet' d = JobSet
    { jobsetCommit :: Maybe Commit
    , jobsetJobsEither :: Either String [ Job' d ]
    }

type JobSet = JobSet' Evaluated
type DeclaredJobSet = JobSet' Declared

jobsetJobs :: JobSet -> [ Job ]
jobsetJobs = either (const []) id . jobsetJobsEither


newtype JobId = JobId [ JobIdPart ]
    deriving (Eq, Ord)

data JobIdPart
    = JobIdName JobName
    | JobIdCommit CommitId
    | JobIdTree TreeId
    deriving (Eq, Ord)

newtype JobRef = JobRef [ Text ]
    deriving (Eq, Ord)

textJobIdPart :: JobIdPart -> Text
textJobIdPart = \case
    JobIdName name -> textJobName name
    JobIdCommit cid -> textCommitId cid
    JobIdTree tid -> textTreeId tid
