module Eval (
    EvalInput(..),
    EvalError(..), textEvalError,
    Eval, runEval,

    evalJob,
    evalJobSet,
    evalJobReference,

    loadJobSetById,
    fillInDependencies,
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.List
import Data.Maybe
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T

import System.FilePath

import Config
import Destination
import Job.Types
import Repo

data EvalInput = EvalInput
    { eiJobRoot :: JobRoot
    , eiRootPath :: FilePath
    , eiCurrentIdRev :: [ JobIdPart ]
    , eiContainingRepo :: Maybe Repo
    , eiOtherRepos :: [ ( RepoName, Repo ) ]
    , eiDestinations :: [ ( DestinationName, Destination ) ]
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
    | all (isJust . jcRepo) (jobCheckout djob) = return False
    | otherwise = asks (not . any matches . eiCurrentIdRev)
  where
    matches (JobIdName _) = False
    matches (JobIdCommit rname _) = isNothing rname
    matches (JobIdTree rname _ _) = isNothing rname

collectOtherRepos :: DeclaredJobSet -> DeclaredJob -> Eval [ ( Maybe ( RepoName, Maybe Text ), FilePath ) ]
collectOtherRepos dset decl = do
    let dependencies = map fst $ jobUses decl
    dependencyRepos <- forM dependencies $ \name -> do
        jobs <- either (throwError . OtherEvalError . T.pack) return $ jobsetJobsEither dset
        job <- maybe (throwError $ OtherEvalError $ "job ‘" <> textJobName name <> "’ not found") return . find ((name ==) . jobName) $ jobs
        return $ jobCheckout job

    missingDefault <- isDefaultRepoMissingInId decl

    let checkouts =
            (if missingDefault then id else (filter (isJust . jcRepo))) $
            concat
                [ jobCheckout decl
                , concat dependencyRepos
                ]
    let commonSubdir reporev = joinPath $ foldr1 commonPrefix $
            map (maybe [] splitDirectories . jcSubtree) . filter ((reporev ==) . jcRepo) $ checkouts
    return $ map (\r -> ( r, commonSubdir r )) . nub . map jcRepo $ checkouts


evalJob :: [ ( Maybe RepoName, Tree ) ] -> DeclaredJobSet -> DeclaredJob -> Eval ( Job, JobSetId )
evalJob revisionOverrides dset decl = do
    EvalInput {..} <- ask
    otherRepos <- collectOtherRepos dset decl
    otherRepoTrees <- forM otherRepos $ \( mbrepo, commonPath ) -> do
        ( mbrepo, ) . ( commonPath, ) <$> do
            case lookup (fst <$> mbrepo) revisionOverrides of
                Just tree -> return tree
                Nothing -> do
                    repo <- evalRepo (fst <$> mbrepo)
                    commit <- readCommit repo (fromMaybe "HEAD" $ join $ snd <$> mbrepo)
                    getSubtree (Just commit) commonPath =<< getCommitTree commit

    checkouts <- forM (jobCheckout decl) $ \dcheckout -> do
        return dcheckout
            { jcRepo =
                fromMaybe (error $ "expecting repo in either otherRepoTrees or revisionOverrides: " <> show (textRepoName . fst <$> jcRepo dcheckout)) $
                msum
                    [ snd <$> lookup (jcRepo dcheckout) otherRepoTrees
                    , lookup (fst <$> jcRepo dcheckout) revisionOverrides
                    ]
            }

    destinations <- forM (jobPublish decl) $ \dpublish -> do
        case lookup (jpDestination dpublish) eiDestinations of
            Just dest -> return $ dpublish { jpDestination = dest }
            Nothing -> throwError $ OtherEvalError $ "no url defined for destination ‘" <> textDestinationName (jpDestination dpublish) <> "’"

    let otherRepoIds = map (\( repo, ( subtree, tree )) -> JobIdTree (fst <$> repo) subtree (treeId tree)) otherRepoTrees
    return
        ( Job
            { jobId = JobId $ reverse $ reverse otherRepoIds ++ JobIdName (jobId decl) : eiCurrentIdRev
            , jobName = jobName decl
            , jobCheckout = checkouts
            , jobRecipe = jobRecipe decl
            , jobArtifacts = jobArtifacts decl
            , jobUses = jobUses decl
            , jobPublish = destinations
            }
        , JobSetId $ reverse $ reverse otherRepoIds ++ eiCurrentIdRev
        )

evalJobSet :: [ ( Maybe RepoName, Tree ) ] -> DeclaredJobSet -> Eval JobSet
evalJobSet revisionOverrides decl = do
    EvalInput {..} <- ask
    jobs <- fmap (fmap (map fst))
        $ either (return . Left) (handleToEither . mapM (evalJob revisionOverrides decl))
        $ jobsetJobsEither decl
    let explicit =
            case liftM2 zip (jobsetJobsEither decl) jobs of
                Left _ -> []
                Right declEval -> catMaybes $
                    map (\jid -> jobId . snd <$> find ((jid ==) . jobId . fst) declEval) $ jobsetExplicitlyRequested decl
    return JobSet
        { jobsetId = JobSetId $ reverse $ eiCurrentIdRev
        , jobsetConfig = jobsetConfig decl
        , jobsetCommit = jobsetCommit decl
        , jobsetExplicitlyRequested = explicit
        , jobsetJobsEither = jobs
        }
  where
    handleToEither = flip catchError (return . Left . T.unpack . textEvalError) . fmap Right

evalRepo :: Maybe RepoName -> Eval Repo
evalRepo Nothing = asks eiContainingRepo >>= \case
    Just repo -> return repo
    Nothing -> throwError $ OtherEvalError $ "no default repo"
evalRepo (Just name) = asks (lookup name . eiOtherRepos) >>= \case
    Just repo -> return repo
    Nothing -> throwError $ OtherEvalError $ "repo ‘" <> textRepoName name <> "’ not defined"


canonicalJobName :: [ Text ] -> Config -> Maybe Tree -> Eval JobSet
canonicalJobName (r : rs) config mbDefaultRepo = do
    let name = JobName r
        dset = JobSet () (Just config) Nothing [] $ Right $ configJobs config
    case find ((name ==) . jobName) (configJobs config) of
        Just djob -> do
            otherRepos <- collectOtherRepos dset djob
            ( overrides, rs' ) <- (\f -> foldM f ( [], rs ) otherRepos) $
                \( overrides, crs ) ( mbrepo, path ) -> do
                    ( tree, crs' ) <- readTreeFromIdRef crs path =<< evalRepo (fst <$> mbrepo)
                    return ( ( fst <$> mbrepo, tree ) : overrides, crs' )
            case rs' of
                (r' : _) -> throwError $ OtherEvalError $ "unexpected job ref part ‘" <> r' <> "’"
                _ -> return ()
            ( job, sid ) <- evalJob (maybe id ((:) . ( Nothing, )) mbDefaultRepo $ overrides) dset djob
            return JobSet
                { jobsetId = sid
                , jobsetConfig = Just config
                , jobsetCommit = Nothing
                , jobsetExplicitlyRequested = []
                , jobsetJobsEither = Right [ job ]
                }
        Nothing -> throwError $ OtherEvalError $ "job ‘" <> r <> "’ not found"
canonicalJobName [] _ _ = throwError $ OtherEvalError "expected job name"

readTreeFromIdRef :: [ Text ] -> FilePath -> Repo -> Eval ( Tree, [ Text ] )
readTreeFromIdRef (r : rs) subdir repo = do
    tryReadCommit repo r >>= \case
        Just commit -> return . (, rs) =<< getSubtree (Just commit) subdir =<< getCommitTree commit
        Nothing -> tryReadTree repo subdir r >>= \case
            Just tree -> return ( tree, rs )
            Nothing -> throwError $ OtherEvalError $ "failed to resolve ‘" <> r <> "’ to a commit or tree in " <> T.pack (show repo)
readTreeFromIdRef [] _ _ = throwError $ OtherEvalError $ "expected commit or tree reference"

canonicalCommitConfig :: [ Text ] -> Repo -> Eval JobSet
canonicalCommitConfig rs repo = do
    ( tree, rs' ) <- readTreeFromIdRef rs "" repo
    config <- either fail return =<< loadConfigForCommit tree
    local (\ei -> ei { eiCurrentIdRev = JobIdTree Nothing "" (treeId tree) : eiCurrentIdRev ei }) $
        canonicalJobName rs' config (Just tree)

evalJobReference :: JobRef -> Eval JobSet
evalJobReference (JobRef rs) =
    asks eiJobRoot >>= \case
        JobRootRepo defRepo -> do
            canonicalCommitConfig rs defRepo
        JobRootConfig config -> do
            canonicalJobName rs config Nothing


jobsetFromConfig :: [ JobIdPart ] -> Config -> Maybe Tree -> Eval ( DeclaredJobSet, [ JobIdPart ], [ ( Maybe RepoName, Tree ) ] )
jobsetFromConfig sid config _ = do
    EvalInput {..} <- ask
    let dset = JobSet () (Just config) Nothing [] $ Right $ configJobs config
    otherRepos <- forM sid $ \case
        JobIdName name -> do
            throwError $ OtherEvalError $ "expected tree id, not a job name ‘" <> textJobName name <> "’"
        JobIdCommit name cid -> do
            repo <- evalRepo name
            tree <- getCommitTree =<< readCommitId repo cid
            return ( name, tree )
        JobIdTree name path tid -> do
            repo <- evalRepo name
            tree <- readTreeId repo path tid
            return ( name, tree )
    return ( dset, eiCurrentIdRev, otherRepos )

jobsetFromCommitConfig :: [ JobIdPart ] -> Repo -> Eval ( DeclaredJobSet, [ JobIdPart ], [ ( Maybe RepoName, Tree ) ] )
jobsetFromCommitConfig (JobIdTree name path tid : sid) repo = do
    when (isJust name) $ do
        throwError $ OtherEvalError $ "expected default repo commit or tree id"
    when (not (null path)) $ do
        throwError $ OtherEvalError $ "expected root commit or tree id"
    tree <- readTreeId repo path tid
    config <- either fail return =<< loadConfigForCommit tree
    local (\ei -> ei { eiCurrentIdRev = JobIdTree Nothing "" (treeId tree) : eiCurrentIdRev ei }) $ do
        ( dset, idRev, otherRepos ) <- jobsetFromConfig sid config (Just tree)
        return ( dset, idRev, ( Nothing, tree ) : otherRepos )

jobsetFromCommitConfig (JobIdCommit name cid : sid) repo = do
    when (isJust name) $ do
        throwError $ OtherEvalError $ "expected default repo commit or tree id"
    tree <- getCommitTree =<< readCommitId repo cid
    jobsetFromCommitConfig (JobIdTree name "" (treeId tree) : sid) repo

jobsetFromCommitConfig (JobIdName name : _) _ = do
    throwError $ OtherEvalError $ "expected commit or tree id, not a job name ‘" <> textJobName name <> "’"

jobsetFromCommitConfig [] _ = do
    throwError $ OtherEvalError $ "expected commit or tree id"

loadJobSetById :: JobSetId -> Eval ( DeclaredJobSet, [ JobIdPart ], [ ( Maybe RepoName, Tree ) ] )
loadJobSetById (JobSetId sid) = do
    asks eiJobRoot >>= \case
        JobRootRepo defRepo -> do
            jobsetFromCommitConfig sid defRepo
        JobRootConfig config -> do
            jobsetFromConfig sid config Nothing

fillInDependencies :: JobSet -> Eval JobSet
fillInDependencies jset = do
    ( dset, idRev, otherRepos ) <- local (\ei -> ei { eiCurrentIdRev = [] }) $ do
        loadJobSetById (jobsetId jset)
    origJobs <- either (throwError . OtherEvalError . T.pack) return $ jobsetJobsEither jset
    declJobs <- either (throwError . OtherEvalError . T.pack) return $ jobsetJobsEither dset
    deps <- gather declJobs S.empty (map jobName origJobs)

    jobs <- local (\ei -> ei { eiCurrentIdRev = idRev }) $ do
        fmap catMaybes $ forM declJobs $ \djob -> if
            | Just job <- find ((jobName djob ==) . jobName) origJobs
            -> return (Just job)

            | jobName djob `S.member` deps
            -> Just . fst <$> evalJob otherRepos dset djob

            | otherwise
            -> return Nothing

    return $ jset { jobsetJobsEither = Right jobs }
  where
    gather djobs cur ( name : rest )
        | name `S.member` cur
        = gather djobs cur rest

        | Just djob <- find ((name ==) . jobName) djobs
        = gather djobs (S.insert name cur) $ map fst (jobUses djob) ++ map (fst . jpArtifact) (jobPublish djob) ++ rest

        | otherwise
        = throwError $ OtherEvalError $ "dependency ‘" <> textJobName name <> "’ not found"

    gather _ cur [] = return cur
