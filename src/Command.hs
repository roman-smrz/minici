module Command (
    CommonOptions(..),
    defaultCommonOptions,

    Command(..),
    CommandArgumentsType(..),

    CommandExec(..),
    tfail,
    CommandInput(..),
    getCommonOptions,
    getRootPath, getJobRoot,
    getRepo, getDefaultRepo, tryGetDefaultRepo,
    getEvalInput, cmdEvalWith,
    getOutput,
    getStorageDir,
) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader

import Data.Kind
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.Console.GetOpt
import System.Exit
import System.IO

import Config
import Destination
import Eval
import Output
import Repo

data CommonOptions = CommonOptions
    { optJobs :: Int
    , optRepo :: [ ( RepoName, FilePath ) ]
    , optDestination :: [ ( DestinationName, Text ) ]
    }

defaultCommonOptions :: CommonOptions
defaultCommonOptions = CommonOptions
    { optJobs = 2
    , optRepo = []
    , optDestination = []
    }

class CommandArgumentsType (CommandArguments c) => Command c where
    commandName :: proxy c -> String
    commandDescription :: proxy c -> String

    type CommandOptions c :: Type
    type CommandOptions c = ()
    commandOptions :: proxy c -> [OptDescr (CommandOptions c -> CommandOptions c)]
    commandOptions _ = []
    defaultCommandOptions :: proxy c -> CommandOptions c
    default defaultCommandOptions :: CommandOptions c ~ () => proxy c -> CommandOptions c
    defaultCommandOptions _ = ()

    type CommandArguments c :: Type
    type CommandArguments c = ()

    commandUsage :: proxy c -> Text

    commandInit :: CommandArgumentsType (CommandArguments c) => proxy c -> CommandOptions c -> CommandArguments c -> c
    commandExec :: c -> CommandExec ()

class CommandArgumentsType args where
    argsFromStrings :: [String] -> Except String args

instance CommandArgumentsType () where
    argsFromStrings [] = return ()
    argsFromStrings _ = throwError "no argument expected"

instance CommandArgumentsType Text where
    argsFromStrings [str] = return $ T.pack str
    argsFromStrings _ = throwError "expected single argument"

instance CommandArgumentsType (Maybe Text) where
    argsFromStrings [] = return $ Nothing
    argsFromStrings [str] = return $ Just (T.pack str)
    argsFromStrings _ = throwError "expected at most one argument"

instance CommandArgumentsType [ Text ] where
    argsFromStrings strs = return $ map T.pack strs


newtype CommandExec a = CommandExec (ReaderT CommandInput IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadFail CommandExec where
    fail = tfail . T.pack

tfail :: Text -> CommandExec a
tfail err = liftIO $ do
    T.hPutStrLn stderr err
    exitFailure

data CommandInput = CommandInput
    { ciOptions :: CommonOptions
    , ciRootPath :: FilePath
    , ciJobRoot :: JobRoot
    , ciContainingRepo :: Maybe Repo
    , ciOtherRepos :: [ ( RepoName, Repo ) ]
    , ciDestinations :: [ ( DestinationName, Destination ) ]
    , ciOutput :: Output
    , ciStorageDir :: FilePath
    }

getCommonOptions :: CommandExec CommonOptions
getCommonOptions = CommandExec (asks ciOptions)

getRootPath :: CommandExec FilePath
getRootPath = CommandExec (asks ciRootPath)

getJobRoot :: CommandExec JobRoot
getJobRoot = CommandExec (asks ciJobRoot)

getRepo :: RepoName -> CommandExec Repo
getRepo name = do
    CommandExec (asks (lookup name . ciOtherRepos)) >>= \case
        Just repo -> return repo
        Nothing -> tfail $ "repo ‘" <> textRepoName name <> "’ not declared"

getDefaultRepo :: CommandExec Repo
getDefaultRepo = do
    tryGetDefaultRepo >>= \case
        Just repo -> return repo
        Nothing -> tfail $ "no default repo"

tryGetDefaultRepo :: CommandExec (Maybe Repo)
tryGetDefaultRepo = CommandExec $ asks ciContainingRepo

getEvalInput :: CommandExec EvalInput
getEvalInput = CommandExec $ do
    eiJobRoot <- asks ciJobRoot
    eiRootPath <- asks ciRootPath
    eiCurrentIdRev <- return []
    eiContainingRepo <- asks ciContainingRepo
    eiOtherRepos <- asks ciOtherRepos
    eiDestinations <- asks ciDestinations
    return EvalInput {..}

cmdEvalWith :: (EvalInput -> EvalInput) -> Eval a -> CommandExec a
cmdEvalWith f ev = do
    either (tfail . textEvalError) return =<< liftIO . runEval ev . f =<< getEvalInput

getOutput :: CommandExec Output
getOutput = CommandExec (asks ciOutput)

getStorageDir :: CommandExec FilePath
getStorageDir = CommandExec (asks ciStorageDir)
