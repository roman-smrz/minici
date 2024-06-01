module Command (
    Command(..),

    CommandExec(..),
    getConfig,
) where

import Control.Monad.Reader

import Config

class Command c where
    commandName :: proxy c -> String

    commandInit :: proxy c -> c
    commandExec :: c -> CommandExec ()


newtype CommandExec a = CommandExec (ReaderT Config IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

getConfig :: CommandExec Config
getConfig = CommandExec ask
