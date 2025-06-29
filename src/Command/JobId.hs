module Command.JobId (
    JobIdCommand,
) where

import Control.Monad
import Control.Monad.IO.Class

import Data.Text (Text)
import Data.Text qualified as T

import System.Console.GetOpt

import Command
import Eval
import Job.Types
import Output
import Repo


data JobIdCommand = JobIdCommand JobIdOptions JobRef

data JobIdOptions = JobIdOptions
    { joVerbose :: Bool
    }

instance Command JobIdCommand where
    commandName _ = "jobid"
    commandDescription _ = "Resolve job reference to canonical job ID"

    type CommandArguments JobIdCommand = Text

    commandUsage _ = T.pack $ unlines $
        [ "Usage: minici jobid [<option>...] <job ref>"
        ]

    type CommandOptions JobIdCommand = JobIdOptions
    defaultCommandOptions _ = JobIdOptions
        { joVerbose = False
        }

    commandOptions _ =
        [ Option [ 'v' ] [ "verbose" ]
            (NoArg $ \opts -> opts { joVerbose = True })
            "show detals of the ID"
        ]

    commandInit _ opts = JobIdCommand opts . parseJobRef
    commandExec = cmdJobId


cmdJobId :: JobIdCommand -> CommandExec ()
cmdJobId (JobIdCommand JobIdOptions {..} ref) = do
    einput <- getEvalInput
    out <- getOutput
    JobId ids <- either (tfail . textEvalError) (return . jobId . fst) =<<
        liftIO (runEval (evalJobReference ref) einput)

    outputMessage out $ textJobId $ JobId ids
    when joVerbose $ do
        outputMessage out ""
        forM_ ids $ \case
            JobIdName name -> outputMessage out $ textJobName name <> " (job name)"
            JobIdCommit mbrepo cid -> outputMessage out $ T.concat
                [ textCommitId cid, " (commit"
                , maybe "" (\name -> " from ‘" <> textRepoName name <> "’ repo") mbrepo
                , ")"
                ]
            JobIdTree mbrepo subtree cid -> outputMessage out $ T.concat
                [ textTreeId cid, " (tree"
                , maybe "" (\name -> " from ‘" <> textRepoName name <> "’ repo") mbrepo
                , if not (null subtree) then ", subtree ‘" <> T.pack subtree <> "’" else ""
                , ")"
                ]
