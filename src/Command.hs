module Command (
    Command(..),
    CommandArgumentsType(..),

    CommandExec(..),
    getConfig,
) where

import Control.Monad.Except
import Control.Monad.Reader

import Data.Kind
import Data.Text (Text)
import Data.Text qualified as T

import System.Console.GetOpt

import Config

class CommandArgumentsType (CommandArguments c) => Command c where
    commandName :: proxy c -> String

    type CommandOptions c :: Type
    type CommandOptions c = ()
    commandOptions :: proxy c -> [OptDescr (CommandOptions c -> CommandOptions c)]
    commandOptions _ = []
    defaultCommandOptions :: proxy c -> CommandOptions c
    default defaultCommandOptions :: CommandOptions c ~ () => proxy c -> CommandOptions c
    defaultCommandOptions _ = ()

    type CommandArguments c :: Type
    type CommandArguments c = ()

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


newtype CommandExec a = CommandExec (ReaderT Config IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

getConfig :: CommandExec Config
getConfig = CommandExec ask
