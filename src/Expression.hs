module Expression (
    StaticRef',
    WatchedRef(..),
    WatchedRef',

    RevisionExpression'(..), RevisionExpression, DeclaredRevisionExpression,
    RangeExpression'(..), RangeExpression, DeclaredRangeExpression,
    CommitRange(..),
    evaluateDeclaredRevision, evaluateDeclaredRange,
    evaluateRevision, evaluateRange,
    getRangeCommits, getAddedRangeCommits,
) where

import Control.Concurrent.STM
import Control.Monad.IO.Class

import Data.Kind
import Data.Maybe
import Data.Text (Text)

import Job.Types
import Output
import Repo


type family StaticRef' d :: Type where
    StaticRef' Declared = Text
    StaticRef' Evaluated = Commit

data WatchedRef
    = BranchRef (STM (Maybe Commit))

type family WatchedRef' d :: Type where
    WatchedRef' Declared = Text
    WatchedRef' Evaluated = WatchedRef


data RevisionExpression' d
    = StaticRef (StaticRef' d)
    | WatchedRef (WatchedRef' d)
    | ModifiedRevision Text (RevisionExpression' d)

type RevisionExpression = RevisionExpression' Evaluated
type DeclaredRevisionExpression = RevisionExpression' Declared


data RangeExpression' d
    = RangeExpression (RevisionExpression' d) {- tip -} (RevisionExpression' d) {- except -}

type RangeExpression = RangeExpression' Evaluated
type DeclaredRangeExpression = RangeExpression' Declared


data CommitRef = CommitRef Text {- suffix -} CommitId
    deriving (Eq)

data CommitRange
    = EmptyCommitRange
    | CommitRange CommitRef {- tip -} CommitRef {- except -}
    deriving (Eq)

textCommitRef :: CommitRef -> Text
textCommitRef (CommitRef suffix cid) = textCommitId cid <> suffix


evaluateDeclaredRevision :: (MonadIO m, MonadFail m, MonadOutput m) => Repo -> DeclaredRevisionExpression -> m RevisionExpression
evaluateDeclaredRevision repo = \case
    StaticRef ref -> StaticRef <$> readCommit repo ref
    WatchedRef ref -> do
        watched <- watchBranch repo ref
        output <- getOutput
        outputEvent output $ TestMessage $ "watch-branch-started " <> ref
        return $ WatchedRef $ BranchRef watched
    ModifiedRevision suffix rev -> ModifiedRevision suffix <$> evaluateDeclaredRevision repo rev

evaluateDeclaredRange :: (MonadIO m, MonadFail m, MonadOutput m) => Repo -> DeclaredRangeExpression -> m RangeExpression
evaluateDeclaredRange repo (RangeExpression a b) =
    RangeExpression <$> evaluateDeclaredRevision repo a <*> evaluateDeclaredRevision repo b

evaluateRevision :: RevisionExpression -> STM (Maybe CommitRef)
evaluateRevision = \case
    StaticRef commit -> return $ Just $ CommitRef "" $ commitId commit
    WatchedRef (BranchRef getter) -> fmap (CommitRef "" . commitId) <$> getter
    ModifiedRevision suffix rev ->
        fmap (\(CommitRef suffix' ref) -> CommitRef (suffix' <> suffix) ref) <$> evaluateRevision rev

evaluateRange :: RangeExpression -> STM CommitRange
evaluateRange (RangeExpression tipExpr exceptExpr) = do
    tip <- evaluateRevision tipExpr
    except <- evaluateRevision exceptExpr
    return $ fromMaybe EmptyCommitRange $ CommitRange <$> tip <*> except

getCommitIdFromRef :: MonadIO m => Repo -> CommitRef -> m (Maybe CommitId)
getCommitIdFromRef repo = liftIO . fmap (fmap commitId) . tryReadCommit repo . textCommitRef

getRangeCommits :: MonadIO m => Repo -> CommitRange -> m [ Commit ]
getRangeCommits _ EmptyCommitRange = return []
getRangeCommits repo (CommitRange tip except) = do
    tipId <- getCommitIdFromRef repo tip
    excId <- getCommitIdFromRef repo except
    listCommitsFrom repo (catMaybes [ tipId ]) (catMaybes [ excId ])

getAddedRangeCommits :: MonadIO m => Repo -> CommitRange -> CommitRange -> m [ Commit ]
getAddedRangeCommits _    _ EmptyCommitRange = return []
getAddedRangeCommits repo EmptyCommitRange r = getRangeCommits repo r
getAddedRangeCommits repo (CommitRange ar br) (CommitRange ar' br') = do
    a <- getCommitIdFromRef repo ar
    b <- getCommitIdFromRef repo br
    a' <- getCommitIdFromRef repo ar'
    b' <- getCommitIdFromRef repo br'
    a'b <- fmap (map commitId) $ mergeBase repo $ catMaybes [ a', b ]
    (++)
        <$> listCommitsFrom repo a'b (catMaybes [ b' ]) -- added by moving B to B'
        <*> listCommitsFrom repo (catMaybes [ a' ]) (catMaybes [ a, b, b' ]) -- added by moving A to A'
