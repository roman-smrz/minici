module Command.Shell (
    ShellCommand,
) where

import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

import System.Environment
import System.Process hiding (ShellCommand)

import Command
import Eval
import Job
import Job.Types


data ShellCommand = ShellCommand JobRef

instance Command ShellCommand where
    commandName _ = "shell"
    commandDescription _ = "Open a shell prepared for given job"

    type CommandArguments ShellCommand = Text

    commandUsage _ = T.unlines $
        [ "Usage: minici shell <job ref>"
        ]

    commandInit _ _ = ShellCommand . parseJobRef
    commandExec = cmdShell


cmdShell :: ShellCommand -> CommandExec ()
cmdShell (ShellCommand ref) = do
    einput <- getEvalInput
    job <- either (tfail . textEvalError) (return . fst) =<<
        liftIO (runEval (evalJobReference ref) einput)
    sh <- fromMaybe "/bin/sh" <$> liftIO (lookupEnv "SHELL")
    storageDir <- getStorageDir
    prepareJob storageDir job $ \checkoutPath -> do
        liftIO $ withCreateProcess (proc sh []) { cwd = Just checkoutPath } $ \_ _ _ ph -> do
            void $ waitForProcess ph
