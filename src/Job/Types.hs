module Job.Types where

import Data.Text (Text)
import Data.Text qualified as T

import System.Process

import Repo


data Job = Job
    { jobName :: JobName
    , jobRecipe :: [ CreateProcess ]
    , jobArtifacts :: [ ( ArtifactName, CreateProcess ) ]
    , jobUses :: [ ( JobName, ArtifactName ) ]
    }

data JobName = JobName Text
    deriving (Eq, Ord, Show)

stringJobName :: JobName -> String
stringJobName (JobName name) = T.unpack name

textJobName :: JobName -> Text
textJobName (JobName name) = name


data ArtifactName = ArtifactName Text
    deriving (Eq, Ord, Show)


data JobSet = JobSet
    { jobsetCommit :: Commit
    , jobsetJobsEither :: Either String [ Job ]
    }

jobsetJobs :: JobSet -> [ Job ]
jobsetJobs = either (const []) id . jobsetJobsEither


newtype JobId = JobId [ JobIdPart ]
    deriving (Eq, Ord)

data JobIdPart
    = JobIdName JobName
    | JobIdCommit CommitId
    | JobIdTree TreeId
    deriving (Eq, Ord)
