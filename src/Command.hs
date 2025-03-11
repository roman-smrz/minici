module Command (
    CommonOptions(..),
    defaultCommonOptions,

    Command(..),
    CommandArgumentsType(..),

    CommandExec(..),
    tfail,
    CommandInput(..),
    getCommonOptions,
    getConfigPath,
    getConfig,
    getRepo, getDefaultRepo,
    getTerminalOutput,
) where

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
import Repo
import Terminal

data CommonOptions = CommonOptions
    { optJobs :: Int
    , optRepo :: [ DeclaredRepo ]
    }

defaultCommonOptions :: CommonOptions
defaultCommonOptions = CommonOptions
    { optJobs = 2
    , optRepo = []
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
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadFail CommandExec where
    fail = tfail . T.pack

tfail :: Text -> CommandExec a
tfail err = liftIO $ do
    T.hPutStrLn stderr err
    exitFailure

data CommandInput = CommandInput
    { ciOptions :: CommonOptions
    , ciConfigPath :: Maybe FilePath
    , ciConfig :: Either String Config
    , ciRepos :: [ ( Maybe RepoName, Repo ) ]
    , ciTerminalOutput :: TerminalOutput
    }

getCommonOptions :: CommandExec CommonOptions
getCommonOptions = CommandExec (asks ciOptions)

getConfigPath :: CommandExec FilePath
getConfigPath = CommandExec $ do
    asks ciConfigPath >>= \case
        Nothing -> fail $ "no job file found"
        Just path -> return path

getConfig :: CommandExec Config
getConfig = CommandExec $ do
    asks ciConfig >>= \case
        Left err -> fail err
        Right config -> return config

getRepo :: RepoName -> CommandExec Repo
getRepo name = CommandExec $ do
    asks (lookup (Just name) . ciRepos) >>= \case
        Just repo -> return repo
        Nothing -> fail $ "repo `" <> showRepoName name <> "' not declared"

getDefaultRepo :: CommandExec Repo
getDefaultRepo = CommandExec $ do
    asks (lookup Nothing . ciRepos) >>= \case
        Just repo -> return repo
        Nothing -> fail $ "no default repo"

getTerminalOutput :: CommandExec TerminalOutput
getTerminalOutput = CommandExec (asks ciTerminalOutput)
