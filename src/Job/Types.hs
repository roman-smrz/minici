module Job.Types where

import Control.Monad.IO.Class

import Data.Containers.ListUtils
import Data.Kind
import Data.Text (Text)
import Data.Text qualified as T

import System.FilePath
import System.FilePath.Glob
import System.Process

import {-# SOURCE #-} Config
import Destination
import Expr
import Repo


data Declared
data Evaluated

type family ExprD d c a :: Type where
    ExprD Declared c a = Expr c a
    ExprD Evaluated c a = a


data Job' d = Job
    { jobId :: JobId' d
    , jobName :: JobName
    , jobCheckout :: [ JobCheckout d ]
    , jobRecipe :: Maybe [ Either CreateProcess Text ]
    , jobArtifacts :: [ ( ArtifactName, Pattern ) ]
    , jobUses :: [ ArtifactSpec d ]
    , jobPublish :: [ JobPublish d ]
    , jobPush :: [ JobPush d ]
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

jobRequiredArtifacts :: Ord (JobId' d) => Job' d -> [ ArtifactSpec d ]
jobRequiredArtifacts job = nubOrd $ jobUses job ++ (map jpArtifact $ jobPublish job)


type family JobRepo d :: Type where
    JobRepo Declared = Maybe ( RepoName, Maybe Text )
    JobRepo Evaluated = Tree

data JobCheckout d = JobCheckout
    { jcRepo :: JobRepo d
    , jcSubtree :: Maybe FilePath
    , jcDestination :: Maybe FilePath
    }

type family JobDestination d :: Type where
    JobDestination Declared = DestinationName
    JobDestination Evaluated = Destination

data JobPublish d = JobPublish
    { jpArtifact :: ArtifactSpec d
    , jpDestination :: JobDestination d
    , jpPath :: Maybe FilePath
    }

data JobPush d = JobPush
    { jpushSource :: ExprD d JobSetContext Commit
    , jpushDestination :: ExprD d JobSetContext Branch
    }


data ArtifactName = ArtifactName Text
    deriving (Eq, Ord, Show)

type ArtifactSpec d = ( JobId' d, ArtifactName )


data JobSet' d = JobSet
    { jobsetId :: JobSetId' d
    , jobsetConfig :: Maybe Config
    , jobsetCommit :: Maybe Commit
    , jobsetExplicitlyRequested :: [ JobId' d ]
    , jobsetJobsEither :: Either String [ Job' d ]
    }

type JobSet = JobSet' Evaluated
type DeclaredJobSet = JobSet' Declared

type family JobSetId' d :: Type where
    JobSetId' Declared = ()
    JobSetId' Evaluated = JobSetId

jobsetJobs :: JobSet -> [ Job ]
jobsetJobs = either (const []) id . jobsetJobsEither


newtype JobId = JobId [ JobIdPart ]
    deriving (Eq, Ord)

newtype JobSetId = JobSetId [ JobIdPart ]
    deriving (Eq, Ord)

data JobIdPart
    = JobIdName JobName
    | JobIdRepo (Maybe RepoName) JobIdRepoPart
    deriving (Eq, Ord)

data JobIdRepoPart
    = JobIdTree FilePath TreeId
    | JobIdCommit CommitId
    | JobIdTag CommitId TagId
    deriving (Eq, Ord)

newtype JobRef = JobRef [ Text ]
    deriving (Eq, Ord)

textJobIdPart :: JobIdPart -> Text
textJobIdPart = \case
    JobIdName name -> textJobName name
    JobIdRepo _ (JobIdTree _ tid) -> textTreeId tid
    JobIdRepo _ (JobIdCommit cid) -> textCommitId cid
    JobIdRepo _ (JobIdTag cid tid) -> textCommitId cid <> "^" <> textTagId tid

textJobId :: JobId -> Text
textJobId (JobId ids) = T.intercalate ":" $ map textJobIdPart ids

parseJobRef :: Text -> JobRef
parseJobRef = JobRef . parseJobRefParts

parseJobRefParts :: Text -> [ Text ]
parseJobRefParts = go [ ':', '.' ] 0 False ""
  where
    go :: [ Char ] -> Int -> Bool -> Text -> Text -> [ Text ]
    go seps plevel pdrop cur s = do
        let bchars | plevel > 0 = [ '(', ')' ]
                   | otherwise  = seps ++ [ '(', ')' ]
        let ( part, rest ) = T.break (`elem` bchars) s
        case T.uncons rest of
            Just ( '.', rest' )
                | Just ( '.', rest'' ) <- T.uncons rest'
                -> go seps plevel pdrop (cur <> part <> "..") rest''
            Just ( sep, rest' )
                | sep `elem` seps
                -> (cur <> part) : go [ sep ] plevel pdrop "" rest'
            Just ( '(', rest' )
                | T.null cur && T.null part
                -> go seps (plevel + 1) True (cur <> part) rest'
                | otherwise
                -> go seps (plevel + 1) pdrop (cur <> part <> "(") rest'
            Just ( ')', rest' )
                | T.null rest' && pdrop
                -> go seps (plevel - 1) False (cur <> part) rest'
                | otherwise
                -> go seps (plevel - 1) pdrop (cur <> part <> ")") rest'
            _   -> [ cur <> part ]

lastJobNameId :: JobId -> Maybe JobName
lastJobNameId (JobId ids) = go Nothing ids
  where
    go _ (JobIdName name : rest) = go (Just name) rest
    go cur (_ : rest) = go cur rest
    go cur [] = cur


data JobSetDep
    = SiblingJobDependency JobName
    | RepoDependency RepoName RepoDepLevel

data RepoDepLevel
    = RepoDepSubtree FilePath
    | RepoDepCommit
    | RepoDepTag

instance Semigroup RepoDepLevel where
    RepoDepTag <> _ = RepoDepTag
    _ <> RepoDepTag = RepoDepTag

    RepoDepCommit <> _ = RepoDepCommit
    _ <> RepoDepCommit = RepoDepCommit

    RepoDepSubtree path <> RepoDepSubtree path' = RepoDepSubtree $
        joinPath $ commonPrefix (splitDirectories path) (splitDirectories path')
      where
        commonPrefix (x : xs) (y : ys) | x == y = x : commonPrefix xs ys
        commonPrefix _        _                 = []

repoDepPath :: RepoDepLevel -> FilePath
repoDepPath = \case
    RepoDepSubtree path -> path
    RepoDepCommit -> ""
    RepoDepTag -> ""


data RepoRef
    = RepoRefTree Tree
    | RepoRefCommit Commit
    | RepoRefTag Commit (Tag Commit)

repoRefRepo :: RepoRef -> Repo
repoRefRepo = \case
    RepoRefTree tree -> treeRepo tree
    RepoRefCommit commit -> commitRepo commit
    RepoRefTag commit _ -> commitRepo commit

repoRefTree :: (MonadIO m, MonadFail m) => RepoRef -> m Tree
repoRefTree = \case
    RepoRefTree tree -> return tree
    RepoRefCommit commit -> getCommitTree commit
    RepoRefTag commit _ -> getCommitTree commit

repoRefToIdPart :: MonadIO m => RepoRef -> m JobIdRepoPart
repoRefToIdPart = \case
    RepoRefTree tree -> return $ JobIdTree (treeSubdir tree) (treeId tree)
    RepoRefCommit commit -> return $ JobIdCommit (commitId commit)
    RepoRefTag commit tag -> return $ JobIdTag (commitId commit) (tagId tag)


data JobSetContext = JobSetContext
    { jscRepos :: [ ( Maybe RepoName, Repo ) ]
    , jscRepoRefs :: [ ( Maybe RepoName, RepoRef ) ]
    }

instance ExprContext JobSetContext where
    type ExprDependency JobSetContext = [ JobSetDep ]
