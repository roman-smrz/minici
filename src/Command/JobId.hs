module Command.JobId (
    JobIdCommand,
) where

import Control.Monad.IO.Class

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Command
import Eval
import Job.Types


data JobIdCommand = JobIdCommand JobRef

instance Command JobIdCommand where
    commandName _ = "jobid"
    commandDescription _ = "Resolve job reference to canonical job ID"

    type CommandArguments JobIdCommand = Text

    commandUsage _ = T.pack $ unlines $
        [ "Usage: minici jobid <job ref>"
        ]

    commandInit _ _ = JobIdCommand . JobRef . T.splitOn "."
    commandExec = cmdJobId


cmdJobId :: JobIdCommand -> CommandExec ()
cmdJobId (JobIdCommand ref) = do
    einput <- getEvalInput
    JobId ids <- either (tfail . textEvalError) return =<<
        liftIO (runEval (evalJobReference ref) einput)

    liftIO $ T.putStrLn $ textJobId $ JobId ids
