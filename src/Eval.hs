module Eval (
    EvalInput(..),
    EvalError(..), textEvalError,
    Eval, runEval,
    RepoRef(..),

    evalJobSet,
    evalJobSetSelected,
    evalJobReference,
    evalJobReferenceToSet,

    loadJobSetById,
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.List
import Data.Maybe
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

repoRefLimit :: RepoDepLevel -> RepoRef -> Eval RepoRef
repoRefLimit (RepoDepSubtree path) rref = do
    tree <- repoRefTree rref
    if  | treeSubdir tree == path -> do
            return $ RepoRefTree tree
        | splitDirectories (treeSubdir tree) `isPrefixOf` splitDirectories path -> do
            tree' <- getSubtree Nothing (makeRelative (treeSubdir tree) path) tree
            return $ RepoRefTree tree'
        | otherwise -> do
            throwError $ OtherEvalError $ "Can't get subtree ‘" <> T.pack path <> "’ from ‘" <> T.pack (treeSubdir tree) <> "’"

repoRefLimit RepoDepCommit rref = case rref of
    RepoRefTree _ -> throwError $ OtherEvalError $ "Can't get commit from subtree ref"
    RepoRefCommit commit -> return $ RepoRefCommit commit
    RepoRefTag commit _ -> return $ RepoRefCommit commit

repoRefLimit RepoDepTag rref = case rref of
    RepoRefTree _ -> throwError $ OtherEvalError $ "Can't get tag from subtree ref"
    RepoRefCommit _ -> throwError $ OtherEvalError $ "Can't get tag from commit ref"
    RepoRefTag commit tag -> return $ RepoRefTag commit tag


checkIfAlreadyHasDefaultRepoId :: Eval Bool
checkIfAlreadyHasDefaultRepoId = do
    asks (any isDefaultRepoId . eiCurrentIdRev)
  where
    isDefaultRepoId (JobIdName _) = False
    isDefaultRepoId (JobIdRepo rname _) = isNothing rname

collectJobSetRepos :: [ ( Maybe RepoName, RepoRef ) ] -> DeclaredJobSet -> Eval [ ( Maybe RepoName, RepoRef ) ]
collectJobSetRepos revisionOverrides dset = do
    jobs <- either (throwError . OtherEvalError . T.pack) return $ jobsetJobsEither dset
    let someJobUsesDefaultRepo = any (any (isNothing . jcRepo) . jobCheckout) jobs
        repos =
            (if someJobUsesDefaultRepo then (Nothing :) else id) $
                map (Just . repoName) $ maybe [] configRepos $ jobsetConfig dset
    forM repos $ \rname -> do
        case lookup rname revisionOverrides of
            Just tree -> return ( rname, tree )
            Nothing -> do
                repo <- evalRepo rname
                commit <- readCommit repo "HEAD"
                return ( rname, RepoRefCommit commit )

collectOtherRepos :: DeclaredJobSet -> DeclaredJob -> Eval [ ( Maybe ( RepoName, Maybe Text ), RepoDepLevel ) ]
collectOtherRepos dset decl = do
    jobs <- either (throwError . OtherEvalError . T.pack) return $ jobsetJobsEither dset
    let gatherDependencies seen (d : ds)
            | d `elem` seen = gatherDependencies seen ds
            | Just job <- find ((d ==) . jobName) jobs
                            = gatherDependencies (d : seen) (map fst (jobRequiredArtifacts job) ++ ds)
            | otherwise     = gatherDependencies (d : seen) ds
        gatherDependencies seen [] = seen

    let dependencies = gatherDependencies [] [ jobName decl ]
    dependencyRepos <- forM dependencies $ \name -> do
        job <- maybe (throwError $ OtherEvalError $ "job ‘" <> textJobName name <> "’ not found") return . find ((name ==) . jobName) $ jobs
        return $ jobCheckout job

    alreadyHasDefaultRepoId <- checkIfAlreadyHasDefaultRepoId
    let checkouts =
            (if alreadyHasDefaultRepoId then filter (isJust . jcRepo) else id) $
                concat dependencyRepos

    let commonSubdir reporev = foldr1 (<>) $
            map (RepoDepSubtree . fromMaybe "" . jcSubtree) . filter ((reporev ==) . jcRepo) $ checkouts
    let canonicalRepoOrder = Nothing : maybe [] (map (Just . repoName) . configRepos) (jobsetConfig dset)
        getCheckoutsForName rname = map (\r -> ( r, commonSubdir r )) $ nub $ filter ((rname ==) . fmap fst) $ map jcRepo checkouts
    return $ concatMap getCheckoutsForName canonicalRepoOrder


evalJobs
    :: [ DeclaredJob ] -> [ Either JobName Job ]
    -> [ ( Maybe RepoName, RepoRef ) ] -> DeclaredJobSet -> [ JobName ] -> Eval [ Job ]
evalJobs _ _ _ JobSet { jobsetJobsEither = Left err } _ = throwError $ OtherEvalError $ T.pack err

evalJobs [] evaluated repos dset@JobSet { jobsetJobsEither = Right decl } (req : reqs)
    | any ((req ==) . either id jobName) evaluated
    = evalJobs [] evaluated repos dset reqs
    | Just d <- find ((req ==) . jobName) decl
    = evalJobs [ d ] evaluated repos dset reqs
    | otherwise
    = throwError $ OtherEvalError $ "job ‘" <> textJobName req <> "’ not found in jobset"
evalJobs [] evaluated _ _ [] = return $ mapMaybe (either (const Nothing) Just) evaluated

evalJobs (current : evaluating) evaluated repos dset reqs
    | any ((jobName current ==) . jobName) evaluating = throwError $ OtherEvalError $ "cyclic dependency when evaluating job ‘" <> textJobName (jobName current) <> "’"
    | any ((jobName current ==) . either id jobName) evaluated = evalJobs evaluating evaluated repos dset reqs

evalJobs (current : evaluating) evaluated repos dset reqs
    | Just missing <- find (`notElem` (jobName current : map (either id jobName) evaluated)) $ map fst $ jobRequiredArtifacts current
    , d <- either (const Nothing) (find ((missing ==) . jobName)) (jobsetJobsEither dset)
    = evalJobs (fromJust d : current : evaluating) evaluated repos dset reqs

evalJobs (current : evaluating) evaluated repos dset reqs = do
    EvalInput {..} <- ask
    otherRepos <- collectOtherRepos dset current
    otherRepoTreesMb <- forM otherRepos $ \( mbrepo, deplevel ) -> do
        Just repoRef <- return $ lookup (fst <$> mbrepo) repos
        let repo = repoRefRepo repoRef
        mbSubtree <- case snd =<< mbrepo of
            Just revisionOverride
                | RepoDepSubtree path <- deplevel
                -> do
                    tree <- getCommitTree =<< readCommit repo revisionOverride
                    return $ Just ( JobIdTree path $ treeId tree, tree )
                | RepoDepCommit <- deplevel
                -> do
                    commit <- readCommit repo revisionOverride
                    tree <- getCommitTree commit
                    return $ Just ( JobIdCommit $ commitId commit, tree )
                | RepoDepTag <- deplevel
                -> do
                    [ cid, tid ] <- return $ T.split (== '^') revisionOverride
                    commit <- readCommit repo cid
                    tag <- readTag repo tid
                    tree <- getCommitTree commit
                    return $ Just ( JobIdTag (commitId commit) (tagId tag), tree )
            Nothing
                -> do
                    repoRef' <- repoRefLimit deplevel repoRef
                    idpart <- repoRefToIdPart repoRef'
                    tree <- repoRefTree repoRef'
                    return $ Just ( idpart, tree )
        return $ fmap (\subtree -> ( mbrepo, subtree )) mbSubtree
    let otherRepoTrees = catMaybes otherRepoTreesMb
    if all isJust otherRepoTreesMb
      then do
        let otherRepoIds = flip mapMaybe otherRepoTrees $ \case
                ( repo, ( idpart, _ ) ) -> do
                    guard $ maybe True (isNothing . snd) repo -- use only checkouts without explicit revision in job id
                    Just $ JobIdRepo (fst <$> repo) idpart
        let currentJobId = JobId $ reverse $ reverse otherRepoIds ++ JobIdName (jobId current) : eiCurrentIdRev

        checkouts <- forM (jobCheckout current) $ \dcheckout -> do
            mbTree <- sequence $ msum
                [ return . snd <$> lookup (jcRepo dcheckout) otherRepoTrees
                , repoRefTree <$> lookup (fst <$> jcRepo dcheckout) repos -- for containing repo if filtered from otherRepos
                ]
            return dcheckout
                { jcRepo =
                    fromMaybe (error $ "expecting repo in either otherRepoTrees or repos: " <> show (textRepoName . fst <$> jcRepo dcheckout)) $ mbTree
                }

        uses <- forM (jobUses current) $ \( jname, aname ) -> do
            Just (Right job) <- return $ find ((jname ==) . either id jobName) evaluated
            return ( jobId job, aname )

        destinations <- forM (jobPublish current) $ \dpublish -> do
            let ( jname, _ ) = jpArtifact dpublish
            jid <- if
                | jname == jobName current -> return currentJobId
                | otherwise -> do
                    Just (Right job) <- return $ find ((jname ==) . either id jobName) evaluated
                    return $ jobId job

            case lookup (jpDestination dpublish) eiDestinations of
                Just dest -> return dpublish
                    { jpArtifact = ( jid, snd (jpArtifact dpublish) )
                    , jpDestination = dest
                    }
                Nothing -> throwError $ OtherEvalError $ "no url defined for destination ‘" <> textDestinationName (jpDestination dpublish) <> "’"

        let job = Job
                { jobId = currentJobId
                , jobName = jobName current
                , jobCheckout = checkouts
                , jobRecipe = jobRecipe current
                , jobArtifacts = jobArtifacts current
                , jobUses = uses
                , jobPublish = destinations
                }
        evalJobs evaluating (Right job : evaluated) repos dset reqs
      else do
        evalJobs evaluating (Left (jobName current) : evaluated) repos dset reqs

evalJobSet :: [ ( Maybe RepoName, RepoRef ) ] -> DeclaredJobSet -> Eval JobSet
evalJobSet revisionOverrides decl = evalJobSetSelected (either (const []) (map jobName) (jobsetJobsEither decl)) revisionOverrides decl

evalJobSetSelected :: [ JobName ] -> [ ( Maybe RepoName, RepoRef ) ] -> DeclaredJobSet -> Eval JobSet
evalJobSetSelected selected revisionOverrides decl = do
    EvalInput {..} <- ask
    repos <- collectJobSetRepos revisionOverrides decl
    alreadyHasDefaultRepoId <- checkIfAlreadyHasDefaultRepoId
    addedRepoIds <-
        mapM (\( mbname, ref ) -> JobIdRepo mbname <$> repoRefToIdPart ref) $
        (if alreadyHasDefaultRepoId then filter (isJust . fst) else id) $
        repos

    evaluated <- handleToEither $ evalJobs [] [] repos decl selected
    let jobs = case liftM2 (,) evaluated (jobsetJobsEither decl) of
            Left err -> Left err
            Right ( ejobs, djobs ) -> Right $ mapMaybe (\dj -> find ((jobName dj ==) . jobName) ejobs) djobs

    let explicit = mapMaybe (\name -> jobId <$> find ((name ==) . jobName) (either (const []) id jobs)) $ jobsetExplicitlyRequested decl
    return JobSet
        { jobsetId = JobSetId $ reverse $ reverse addedRepoIds ++ eiCurrentIdRev
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


canonicalJobName :: [ Text ] -> Config -> Maybe RepoRef -> Eval JobSet
canonicalJobName (r : rs) config mbDefaultRepo = do
    let name = JobName r
        dset = JobSet
            { jobsetId = ()
            , jobsetConfig = Just config
            , jobsetCommit = Nothing
            , jobsetExplicitlyRequested = [ name ]
            , jobsetJobsEither = Right $ configJobs config
            }
    case find ((name ==) . jobName) (configJobs config) of
        Just djob -> do
            otherRepos <- collectOtherRepos dset djob
            ( overrides, rs' ) <- (\f -> foldM f ( [], rs ) otherRepos) $
                \( overrides, crs ) ( mbrepo, deplevel ) -> if
                    | Just ( _, Just _ ) <- mbrepo -> do
                        -- use only checkouts without explicit revision in job id
                        return ( overrides, crs )
                    | otherwise -> do
                        ( repoRef, crs' ) <- readRepoRefFromIdRef crs (repoDepPath deplevel) =<< evalRepo (fst <$> mbrepo)
                        ref' <- repoRefLimit deplevel repoRef
                        return ( ( fst <$> mbrepo, ref' ) : overrides, crs' )
            case rs' of
                (r' : _) -> throwError $ OtherEvalError $ "unexpected job ref part ‘" <> r' <> "’"
                _ -> return ()
            evalJobSetSelected (jobsetExplicitlyRequested dset) (maybe id ((:) . ( Nothing, )) mbDefaultRepo $ overrides) dset
        Nothing -> throwError $ OtherEvalError $ "job ‘" <> r <> "’ not found"
canonicalJobName [] _ _ = throwError $ OtherEvalError "expected job name"

readRepoRefFromIdRef :: [ Text ] -> FilePath -> Repo -> Eval ( RepoRef, [ Text ] )
readRepoRefFromIdRef (r : rs) subdir repo = do
    tryReadCommit repo r >>= \case
        Just commit
            | subdir == "" -> return ( RepoRefCommit commit, rs )
            | otherwise -> return . ( , rs ) . RepoRefTree =<< getSubtree (Just commit) subdir =<< getCommitTree commit
        Nothing -> tryReadTree repo subdir r >>= \case
            Just tree -> return ( RepoRefTree tree, rs )
            Nothing -> throwError $ OtherEvalError $ "failed to resolve ‘" <> r <> "’ to a commit or tree in " <> T.pack (show repo)
readRepoRefFromIdRef [] _ _ = throwError $ OtherEvalError $ "expected commit or tree reference"

canonicalCommitConfig :: [ Text ] -> Repo -> Eval JobSet
canonicalCommitConfig rs repo = do
    ( rref, rs' ) <- readRepoRefFromIdRef rs "" repo
    tree <- repoRefTree rref
    config <- either fail return =<< loadConfigForCommit tree
    local (\ei -> ei { eiCurrentIdRev = JobIdRepo Nothing (JobIdTree "" (treeId tree)) : eiCurrentIdRev ei }) $
        canonicalJobName rs' config (Just rref)

evalJobReferenceToSet :: JobRef -> Eval JobSet
evalJobReferenceToSet (JobRef rs) =
    asks eiJobRoot >>= \case
        JobRootRepo defRepo -> do
            canonicalCommitConfig rs defRepo
        JobRootConfig config -> do
            canonicalJobName rs config Nothing

evalJobReference :: JobRef -> Eval Job
evalJobReference ref = do
    jset <- evalJobReferenceToSet ref
    jobs <- either (throwError . OtherEvalError . T.pack) return $ jobsetJobsEither jset
    [ name ] <- return $ jobsetExplicitlyRequested jset
    maybe (error "missing job in evalJobReferenceToSet result") return $ find ((name ==) . jobId) jobs


jobsetFromConfig :: [ JobIdPart ] -> Config -> Maybe Tree -> Eval ( DeclaredJobSet, [ JobIdPart ], [ ( Maybe RepoName, Tree ) ] )
jobsetFromConfig sid config _ = do
    EvalInput {..} <- ask
    let dset = JobSet () (Just config) Nothing [] $ Right $ configJobs config
    otherRepos <- forM sid $ \case
        JobIdName name -> do
            throwError $ OtherEvalError $ "expected tree id, not a job name ‘" <> textJobName name <> "’"
        JobIdRepo name repoId -> do
            repo <- evalRepo name
            tree <- case repoId of
                JobIdTree path tid -> readTreeId repo path tid
                JobIdCommit cid -> getCommitTree =<< readCommitId repo cid
                JobIdTag cid _ -> getCommitTree =<< readCommitId repo cid
            return ( name, tree )
    return ( dset, eiCurrentIdRev, otherRepos )

jobsetFromCommitConfig :: [ JobIdPart ] -> Repo -> Eval ( DeclaredJobSet, [ JobIdPart ], [ ( Maybe RepoName, Tree ) ] )
jobsetFromCommitConfig (JobIdRepo name (JobIdTree path tid) : sid) repo = do
    when (isJust name) $ do
        throwError $ OtherEvalError $ "expected default repo commit or tree id"
    when (not (null path)) $ do
        throwError $ OtherEvalError $ "expected root commit or tree id"
    tree <- readTreeId repo path tid
    config <- either fail return =<< loadConfigForCommit tree
    local (\ei -> ei { eiCurrentIdRev = JobIdRepo Nothing (JobIdTree (treeSubdir tree) (treeId tree)) : eiCurrentIdRev ei }) $ do
        ( dset, idRev, otherRepos ) <- jobsetFromConfig sid config (Just tree)
        return ( dset, idRev, ( Nothing, tree ) : otherRepos )

jobsetFromCommitConfig (JobIdRepo name (JobIdCommit cid) : sid) repo = do
    when (isJust name) $ do
        throwError $ OtherEvalError $ "expected default repo commit or tree id"
    tree <- getCommitTree =<< readCommitId repo cid
    jobsetFromCommitConfig (JobIdRepo name (JobIdTree (treeSubdir tree) (treeId tree)) : sid) repo

jobsetFromCommitConfig (JobIdRepo name (JobIdTag cid _) : sid) repo = do
    when (isJust name) $ do
        throwError $ OtherEvalError $ "expected default repo commit or tree id"
    tree <- getCommitTree =<< readCommitId repo cid
    jobsetFromCommitConfig (JobIdRepo name (JobIdTree (treeSubdir tree) (treeId tree)) : sid) repo

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
