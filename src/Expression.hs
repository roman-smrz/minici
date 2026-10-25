module Expression (
    DeclaredRef(..),
    StaticRef',
    WatchedRef(..),
    WatchedRef',

    RevisionExpression'(..), RevisionExpression, DeclaredRevisionExpression,
    RangeExpression'(..), RangeExpression, DeclaredRangeExpression,
    CommitRange(..),
    evaluateDeclaredRevision, evaluateDeclaredRange,
    evaluateRevision, evaluateRange,
    getRangeCommits, getAddedRangeCommits,

    isRangeExpressionDynamic,
    isRevisionExpressionDynamic,

    parseRangeExpression,
) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

import Data.Bifunctor
import Data.Char
import Data.Function
import Data.Kind
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

import System.FilePath.Glob as Glob
import System.IO.Error

import Job.Types
import Output
import Repo


data DeclaredRef
    = TextRef Text
    | TagRef Pattern (Tag Commit -> Tag Commit -> Tag Commit)

type family StaticRef' d :: Type where
    StaticRef' Declared = DeclaredRef
    StaticRef' Evaluated = Commit

data WatchedRef
    = BranchRef (STM (Maybe Commit))
    | WatchedTagRef (WatchedTags (Tag Commit))

type family WatchedRef' d :: Type where
    WatchedRef' Declared = DeclaredRef
    WatchedRef' Evaluated = WatchedRef


type family StaticTags' d :: Type where
    StaticTags' Declared = Pattern
    StaticTags' Evaluated = [ Tag Commit ]

data WatchedTags a = WatchedTags
    { wtPattern :: Pattern
    , wtUpdate :: Tag Commit -> a -> a
    , wtValue :: TVar a
    , wtChan :: TChan (Tag Commit)
    }

type family WatchedTags' d :: Type where
    WatchedTags' Declared = Pattern
    WatchedTags' Evaluated = WatchedTags [ Tag Commit ]


data RevisionExpression' d
    = StaticRef (StaticRef' d)
    | WatchedRef (WatchedRef' d)
    | ModifiedRevision Text (RevisionExpression' d)

type RevisionExpression = RevisionExpression' Evaluated
type DeclaredRevisionExpression = RevisionExpression' Declared

watchedRevision :: DeclaredRevisionExpression -> DeclaredRevisionExpression
watchedRevision = \case
    StaticRef r -> WatchedRef r
    WatchedRef r -> WatchedRef r
    ModifiedRevision m r -> ModifiedRevision m (watchedRevision r)


data RangeExpression' d
    = SingleRevisionExpression (RevisionExpression' d)
    | RangeExpression (RevisionExpression' d) {- tip -} (RevisionExpression' d) {- except -}
    | StaticTagsExpression (StaticTags' d)
    | WatchedTagsExpression (WatchedTags' d)

type RangeExpression = RangeExpression' Evaluated
type DeclaredRangeExpression = RangeExpression' Declared

watchedRange :: DeclaredRangeExpression -> DeclaredRangeExpression
watchedRange = \case
    SingleRevisionExpression rev -> SingleRevisionExpression (watchedRevision rev)
    RangeExpression r1 r2 -> RangeExpression (watchedRevision r1) (watchedRevision r2)
    StaticTagsExpression pat -> WatchedTagsExpression pat
    WatchedTagsExpression pat -> WatchedTagsExpression pat


data CommitRef = CommitRef Text {- suffix -} CommitId
    deriving (Eq)

data CommitRange
    = EmptyCommitRange
    | CommitRange CommitRef {- tip -} CommitRef {- except -}
    | CommitSet [ CommitRef ]
    deriving (Eq)

textCommitRef :: CommitRef -> Text
textCommitRef (CommitRef suffix cid) = textCommitId cid <> suffix


evaluateDeclaredRevision :: (MonadIO m, MonadFail m, MonadOutput m) => Repo -> DeclaredRevisionExpression -> m RevisionExpression
evaluateDeclaredRevision repo = \case
    StaticRef (TextRef ref) -> StaticRef <$> readCommit repo ref
    StaticRef (TagRef pat sel) -> do
        filter (Glob.match pat . T.unpack . tagTag) <$> listTags repo >>= \case
            [] -> fail $ "no tag matching " <> show (decompile pat) <> " found"
            ts -> return $ StaticRef $ tagObject $ foldl1' sel ts

    WatchedRef (TextRef ref) -> do
        watched <- watchBranch repo ref
        output <- getOutput
        outputEvent output $ TestMessage $ "watch-branch-started " <> ref
        return $ WatchedRef $ BranchRef watched
    WatchedRef (TagRef wtPattern wtUpdate) -> do
        wtChan <- watchTags repo
        value <- filter (Glob.match wtPattern . T.unpack . tagTag) <$> listTags repo >>= \case
            [] -> fail $ "no tag matching " <> show (decompile wtPattern) <> " found"
            ts -> return $ foldl1' wtUpdate ts
        wtValue <- liftIO $ newTVarIO value
        return $ WatchedRef $ WatchedTagRef WatchedTags {..}

    ModifiedRevision suffix rev -> ModifiedRevision suffix <$> evaluateDeclaredRevision repo rev

evaluateDeclaredRange :: (MonadIO m, MonadFail m, MonadOutput m) => Repo -> DeclaredRangeExpression -> m RangeExpression
evaluateDeclaredRange repo = \case
    SingleRevisionExpression a ->
        SingleRevisionExpression <$> evaluateDeclaredRevision repo a
    RangeExpression a b ->
        RangeExpression <$> evaluateDeclaredRevision repo a <*> evaluateDeclaredRevision repo b
    StaticTagsExpression pat ->
        StaticTagsExpression . filter (Glob.match pat . T.unpack . tagTag) <$> listTags repo
    WatchedTagsExpression wtPattern -> do
        wtChan <- watchTags repo
        wtValue <- liftIO . newTVarIO . filter (Glob.match wtPattern . T.unpack . tagTag) =<< listTags repo
        let wtUpdate = (:)
        return $ WatchedTagsExpression WatchedTags {..}

evaluateRevision :: RevisionExpression -> STM (Maybe CommitRef)
evaluateRevision = \case
    StaticRef commit -> return $ Just $ CommitRef "" $ commitId commit
    WatchedRef (BranchRef getter) -> fmap (CommitRef "" . commitId) <$> getter
    WatchedRef (WatchedTagRef wt) -> Just . CommitRef "" . commitId . tagObject <$> updateWatchedTags wt
    ModifiedRevision suffix rev ->
        fmap (\(CommitRef suffix' ref) -> CommitRef (suffix' <> suffix) ref) <$> evaluateRevision rev

evaluateRange :: RangeExpression -> STM CommitRange
evaluateRange = \case
    SingleRevisionExpression revExpr -> do
        rev <- evaluateRevision revExpr
        return $ fromMaybe EmptyCommitRange $ CommitSet . (: []) <$> rev
    RangeExpression tipExpr exceptExpr -> do
        tip <- evaluateRevision tipExpr
        except <- evaluateRevision exceptExpr
        return $ fromMaybe EmptyCommitRange $ CommitRange <$> tip <*> except
    StaticTagsExpression tags -> do
        return $ CommitSet $ map (CommitRef "" . commitId . tagObject) tags
    WatchedTagsExpression wt -> do
        CommitSet . map (CommitRef "" . commitId . tagObject) <$> updateWatchedTags wt

getCommitIdFromRef :: MonadIO m => Repo -> CommitRef -> m (Maybe CommitId)
getCommitIdFromRef repo = liftIO . fmap (fmap commitId) . tryReadCommit repo . textCommitRef

getRangeCommits :: MonadIO m => Repo -> CommitRange -> m [ Commit ]
getRangeCommits repo = \case
    EmptyCommitRange -> do
        return []
    CommitRange tip except -> do
        tipId <- getCommitIdFromRef repo tip
        excId <- getCommitIdFromRef repo except
        listCommitsFrom repo (catMaybes [ tipId ]) (catMaybes [ excId ])
    CommitSet refs -> do
        return . catMaybes . map (either (const Nothing) Just) =<<
            mapM (liftIO . tryIOError . readCommitId repo) . catMaybes =<<
                mapM (getCommitIdFromRef repo) refs

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
getAddedRangeCommits repo prev cur = do
    prevList <- getRangeCommits repo prev
    curList <- getRangeCommits repo cur
    return $ filter (not . (`elem` prevList)) curList


isRangeExpressionDynamic :: RangeExpression -> Bool
isRangeExpressionDynamic = \case
    SingleRevisionExpression x -> isRevisionExpressionDynamic x
    RangeExpression x y -> isRevisionExpressionDynamic x || isRevisionExpressionDynamic y
    StaticTagsExpression {} -> False
    WatchedTagsExpression {} -> True

isRevisionExpressionDynamic :: RevisionExpression -> Bool
isRevisionExpressionDynamic = \case
    StaticRef _ -> False
    WatchedRef _ -> True
    ModifiedRevision _ e -> isRevisionExpressionDynamic e

updateWatchedTags :: WatchedTags a -> STM a
updateWatchedTags wt@WatchedTags {..} = do
    tryReadTChan wtChan >>= \case
        Nothing -> readTVar wtValue
        Just tag -> do
            when (Glob.match wtPattern $ T.unpack $ tagTag tag) $ do
                writeTVar wtValue . (wtUpdate tag) =<< readTVar wtValue
            updateWatchedTags wt


type ExprParser = Parsec Void Text

parseRangeExpression :: Text -> Either String DeclaredRangeExpression
parseRangeExpression = bimap errorBundlePretty id . runParser parser ""
  where
    parser :: ExprParser DeclaredRangeExpression
    parser = do
        range <- parseRange
        eof
        return range

    parseRange :: ExprParser DeclaredRangeExpression
    parseRange = do
        range <- parseRevision
        choice
          [ do
              SingleRevisionExpression rev1 <- return range
              _ <- string ".."
              SingleRevisionExpression rev2 <- parseRevision
              return $ RangeExpression rev2 rev1
          , do
              return range
          ]

    parseRevision :: ExprParser DeclaredRangeExpression
    parseRevision = do
        tok <- fmap T.concat $ some $ choice
            [ takeWhile1P Nothing (\x -> isAlphaNum x || x `elem` [ '_', '-', '/' ])
            , try $ string "." <* notFollowedBy (char '.')
            ]

        range <- choice
            [ between (char '(') (char ')') $ do
                case tok of
                    "watch" -> watchedRange <$> parseRange
                    "tags" -> StaticTagsExpression <$> parseTagPattern
                    "highesttag" -> do
                        pat <- parseTagPattern
                        return $ SingleRevisionExpression $ StaticRef $ TagRef pat higherTag
                    _ -> unexpected $ Tokens $ NE.fromList $ chunkToTokens @Text Proxy tok

            , do
                return $ SingleRevisionExpression $ StaticRef $ TextRef tok
            ]

        choice
            [ do
                SingleRevisionExpression rev <- return range
                suffix <- takeWhile1P Nothing (\x -> isNumber x || x `elem` [ '~', '^', '@', '{', '}' ])
                return $ SingleRevisionExpression $ ModifiedRevision suffix rev
            , do
                return range
            ]

    parseTagPattern = compile . T.unpack <$> takeWhile1P Nothing (\x -> isAlphaNum x || x `elem` [ '_', '-', '.', '[', ']', '*', '?' ])


higherTag :: Tag a -> Tag a -> Tag a
higherTag t t'
    | components t < components t' = t'
    | otherwise                    = t
  where
    components = map component . T.groupBy ((==) `on` isDigit) . tagTag
    component s
        | Just ( c, _ ) <- T.uncons s, isDigit c
        = Left $ read @Integer $ T.unpack s
        | otherwise
        = Right s
