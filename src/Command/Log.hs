module Command.Log (
    LogCommand,
) where

import Control.Monad.IO.Class

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL

import System.FilePath

import Command
import Eval
import Job
import Job.Types
import Output


data LogCommand = LogCommand JobRef

instance Command LogCommand where
    commandName _ = "log"
    commandDescription _ = "Show log for the given job"

    type CommandArguments LogCommand = Text

    commandUsage _ = T.pack $ unlines $
        [ "Usage: minici log <job ref>"
        ]

    commandInit _ _ = LogCommand . parseJobRef
    commandExec = cmdLog


cmdLog :: LogCommand -> CommandExec ()
cmdLog (LogCommand ref) = do
    einput <- getEvalInput
    jid <- either (tfail . textEvalError) (return . jobId . fst) =<<
        liftIO (runEval (evalJobReference ref) einput)
    output <- getOutput
    storageDir <- getStorageDir
    liftIO $ mapM_ (outputEvent output . OutputMessage . TL.toStrict) . TL.lines =<<
        TL.readFile (storageDir </> jobStorageSubdir jid </> "log")
