module Command (
    CommonOptions(..),
    defaultCommonOptions,

    Command(..),
    CommandArgumentsType(..),

    CommandExec(..),
    CommandInput(..),
    getCommonOptions,
    getConfig,
    getTerminalOutput,
) where

import Control.Monad.Except
import Control.Monad.Reader

import Data.Kind
import Data.Text (Text)
import Data.Text qualified as T

import System.Console.GetOpt

import Config
import Terminal

data CommonOptions = CommonOptions
    { optJobs :: Int
    }

defaultCommonOptions :: CommonOptions
defaultCommonOptions = CommonOptions
    { optJobs = 2
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


newtype CommandExec a = CommandExec (ReaderT CommandInput IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

data CommandInput = CommandInput
    { ciOptions :: CommonOptions
    , ciConfig :: Config
    , ciTerminalOutput :: TerminalOutput
    }

getCommonOptions :: CommandExec CommonOptions
getCommonOptions = CommandExec (asks ciOptions)

getConfig :: CommandExec Config
getConfig = CommandExec (asks ciConfig)

getTerminalOutput :: CommandExec TerminalOutput
getTerminalOutput = CommandExec (asks ciTerminalOutput)
