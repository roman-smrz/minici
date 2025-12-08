module Eval (
    EvalInput(..),
    EvalError(..), textEvalError,
    Eval, runEval,

    evalJobSet,
    evalJobSetSelected,
    evalJobReference,

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


commonPrefix :: Eq a => [ a ] -> [ a ] -> [ a ]
commonPrefix (x : xs) (y : ys) | x == y = x : commonPrefix xs ys
commonPrefix _        _                 = []

checkIfAlreadyHasDefaultRepoId :: Eval Bool
checkIfAlreadyHasDefaultRepoId = do
    asks (any isDefaultRepoId . eiCurrentIdRev)
  where
    isDefaultRepoId (JobIdName _) = False
    isDefaultRepoId (JobIdCommit rname _) = isNothing rname
    isDefaultRepoId (JobIdTree rname _ _) = isNothing rname

collectJobSetRepos :: [ ( Maybe RepoName, Tree ) ] -> DeclaredJobSet -> Eval [ ( Maybe RepoName, Tree ) ]
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
                tree <- getCommitTree =<< readCommit repo "HEAD"
                return ( rname, tree )

collectOtherRepos :: DeclaredJobSet -> DeclaredJob -> Eval [ ( Maybe ( RepoName, Maybe Text ), FilePath ) ]
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

    let commonSubdir reporev = joinPath $ foldr1 commonPrefix $
            map (maybe [] splitDirectories . jcSubtree) . filter ((reporev ==) . jcRepo) $ checkouts
    let canonicalRepoOrder = Nothing : maybe [] (map (Just . repoName) . configRepos) (jobsetConfig dset)
        getCheckoutsForName rname = map (\r -> ( r, commonSubdir r )) $ nub $ filter ((rname ==) . fmap fst) $ map jcRepo checkouts
    return $ concatMap getCheckoutsForName canonicalRepoOrder


evalJobs
    :: [ DeclaredJob ] -> [ Either JobName Job ]
    -> [ ( Maybe RepoName, Tree ) ] -> DeclaredJobSet -> [ JobName ] -> Eval [ Job ]
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
    otherRepoTreesMb <- forM otherRepos $ \( mbrepo, commonPath ) -> do
        Just tree <- return $ lookup (fst <$> mbrepo) repos
        mbSubtree <- case snd =<< mbrepo of
            Just revisionOverride -> return . Just =<< getCommitTree =<< readCommit (treeRepo tree) revisionOverride
            Nothing
                | treeSubdir tree == commonPath -> do
                    return $ Just tree
                | splitDirectories (treeSubdir tree) `isPrefixOf` splitDirectories commonPath -> do
                    Just <$> getSubtree Nothing (makeRelative (treeSubdir tree) commonPath) tree
                | otherwise -> do
                    return Nothing
        return $ fmap (\subtree -> ( mbrepo, ( commonPath, subtree ) )) mbSubtree
    let otherRepoTrees = catMaybes otherRepoTreesMb
    if all isJust otherRepoTreesMb
      then do
        checkouts <- forM (jobCheckout current) $ \dcheckout -> do
            return dcheckout
                { jcRepo =
                    fromMaybe (error $ "expecting repo in either otherRepoTrees or repos: " <> show (textRepoName . fst <$> jcRepo dcheckout)) $
                    msum
                        [ snd <$> lookup (jcRepo dcheckout) otherRepoTrees
                        , lookup (fst <$> jcRepo dcheckout) repos -- for containing repo if filtered from otherRepos
                        ]
                }

        destinations <- forM (jobPublish current) $ \dpublish -> do
            case lookup (jpDestination dpublish) eiDestinations of
                Just dest -> return $ dpublish { jpDestination = dest }
                Nothing -> throwError $ OtherEvalError $ "no url defined for destination ‘" <> textDestinationName (jpDestination dpublish) <> "’"

        let otherRepoIds = flip mapMaybe otherRepoTrees $ \case
                ( repo, ( subtree, tree )) -> do
                    guard $ maybe True (isNothing . snd) repo -- use only checkouts without explicit revision in job id
                    Just $ JobIdTree (fst <$> repo) subtree (treeId tree)
        let job = Job
                { jobId = JobId $ reverse $ reverse otherRepoIds ++ JobIdName (jobId current) : eiCurrentIdRev
                , jobName = jobName current
                , jobCheckout = checkouts
                , jobRecipe = jobRecipe current
                , jobArtifacts = jobArtifacts current
                , jobUses = jobUses current
                , jobPublish = destinations
                }
        evalJobs evaluating (Right job : evaluated) repos dset reqs
      else do
        evalJobs evaluating (Left (jobName current) : evaluated) repos dset reqs

evalJobSet :: [ ( Maybe RepoName, Tree ) ] -> DeclaredJobSet -> Eval JobSet
evalJobSet revisionOverrides decl = evalJobSetSelected (either (const []) (map jobName) (jobsetJobsEither decl)) revisionOverrides decl

evalJobSetSelected :: [ JobName ] -> [ ( Maybe RepoName, Tree ) ] -> DeclaredJobSet -> Eval JobSet
evalJobSetSelected selected revisionOverrides decl = do
    EvalInput {..} <- ask
    repos <- collectJobSetRepos revisionOverrides decl
    alreadyHasDefaultRepoId <- checkIfAlreadyHasDefaultRepoId
    let addedRepoIds =
            map (\( mbname, tree ) -> JobIdTree mbname (treeSubdir tree) (treeId tree)) $
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


canonicalJobName :: [ Text ] -> Config -> Maybe Tree -> Eval JobSet
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
                \( overrides, crs ) ( mbrepo, path ) -> if
                    | Just ( _, Just _ ) <- mbrepo -> do
                        -- use only checkouts without explicit revision in job id
                        return ( overrides, crs )
                    | otherwise -> do
                        ( tree, crs' ) <- readTreeFromIdRef crs path =<< evalRepo (fst <$> mbrepo)
                        return ( ( fst <$> mbrepo, tree ) : overrides, crs' )
            case rs' of
                (r' : _) -> throwError $ OtherEvalError $ "unexpected job ref part ‘" <> r' <> "’"
                _ -> return ()
            evalJobSetSelected (jobsetExplicitlyRequested dset) (maybe id ((:) . ( Nothing, )) mbDefaultRepo $ overrides) dset
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
    local (\ei -> ei { eiCurrentIdRev = JobIdTree Nothing (treeSubdir tree) (treeId tree) : eiCurrentIdRev ei }) $ do
        ( dset, idRev, otherRepos ) <- jobsetFromConfig sid config (Just tree)
        return ( dset, idRev, ( Nothing, tree ) : otherRepos )

jobsetFromCommitConfig (JobIdCommit name cid : sid) repo = do
    when (isJust name) $ do
        throwError $ OtherEvalError $ "expected default repo commit or tree id"
    tree <- getCommitTree =<< readCommitId repo cid
    jobsetFromCommitConfig (JobIdTree name (treeSubdir tree) (treeId tree) : sid) repo

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
