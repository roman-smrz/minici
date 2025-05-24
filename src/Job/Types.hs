module Job.Types where

import Data.Kind
import Data.Text (Text)
import Data.Text qualified as T

import System.FilePath.Glob
import System.Process

import Repo


data Declared
data Evaluated

data Job' d = Job
    { jobId :: JobId' d
    , jobName :: JobName
    , jobCheckout :: [ JobCheckout d ]
    , jobRecipe :: [ CreateProcess ]
    , jobArtifacts :: [ ( ArtifactName, Pattern ) ]
    , jobUses :: [ ( JobName, ArtifactName ) ]
    }

type Job = Job' Evaluated
type DeclaredJob = Job' Declared

type family JobId' d :: Type where
    JobId' Declared = JobName
    JobId' Evaluated = JobId

data JobName = JobName Text
    deriving (Eq, Ord, Show)

stringJobName :: JobName -> String
stringJobName (JobName name) = T.unpack name

textJobName :: JobName -> Text
textJobName (JobName name) = name


type family JobRepo d :: Type where
    JobRepo Declared = Maybe ( RepoName, Maybe Text )
    JobRepo Evaluated = Tree

data JobCheckout d = JobCheckout
    { jcRepo :: JobRepo d
    , jcSubtree :: Maybe FilePath
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
    | JobIdCommit (Maybe RepoName) CommitId
    | JobIdTree (Maybe RepoName) FilePath TreeId
    deriving (Eq, Ord)

newtype JobRef = JobRef [ Text ]
    deriving (Eq, Ord)

textJobIdPart :: JobIdPart -> Text
textJobIdPart = \case
    JobIdName name -> textJobName name
    JobIdCommit _ cid -> textCommitId cid
    JobIdTree _ _ tid -> textTreeId tid

textJobId :: JobId -> Text
textJobId (JobId ids) = T.intercalate "." $ map textJobIdPart ids

parseJobRef :: Text -> JobRef
parseJobRef = JobRef . go 0 ""
  where
    go :: Int -> Text -> Text -> [ Text ]
    go plevel cur s = do
        let bchars | plevel > 0 = [ '(', ')' ]
                   | otherwise  = [ '.', '(', ')' ]
        let ( part, rest ) = T.break (`elem` bchars) s
        case T.uncons rest of
            Just ( '.', rest' ) -> (cur <> part) : go plevel "" rest'
            Just ( '(', rest' ) -> go (plevel + 1) (cur <> part) rest'
            Just ( ')', rest' ) -> go (plevel - 1) (cur <> part) rest'
            _                   -> [ cur <> part ]
