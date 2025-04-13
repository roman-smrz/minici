module Output (
    Output,
    OutputType(..),
    OutputEvent(..),
    OutputFootnote(..),

    withOutput,
    outputTerminal,
    outputEvent,
    outputFootnote,
) where

import Control.Monad
import Control.Monad.Catch

import Data.Text (Text)
import Data.Text.IO qualified as T

import System.IO

import Job.Types
import Terminal


data Output = Output
    { outTerminal :: Maybe TerminalOutput
    , outLogs :: [ Handle ]
    , outTest :: [ Handle ]
    }

data OutputType
    = TerminalOutput
    | LogOutput FilePath
    | TestOutput FilePath
    deriving (Eq, Ord)

data OutputEvent
    = OutputMessage Text
    | JobStarted JobId
    | JobFinished JobId Text

data OutputFootnote = OutputFootnote
    { footnoteText :: Text
    , footnoteTerminal :: Maybe TerminalFootnote
    }
    deriving (Eq)


withOutput :: [ OutputType ] -> (Output -> IO a) -> IO a
withOutput types inner = go types (Output Nothing [] [])
  where
    go (TerminalOutput : ts) out = do
        term <- initTerminalOutput
        go ts out { outTerminal = Just term }
    go (LogOutput path : ts) out = withOutputFile path $ \h -> do
        go ts out { outLogs = h : outLogs out }
    go (TestOutput path : ts) out = withOutputFile path $ \h -> do
        go ts out { outTest = h : outTest out }
    go [] out = inner out

    withOutputFile "-" f = hSetBuffering stdout LineBuffering >> f stdout
    withOutputFile path f = bracket (openFile' path) hClose f
    openFile' path = do
        h <- openFile path WriteMode
        hSetBuffering h LineBuffering
        return h


outputTerminal :: Output -> Maybe TerminalOutput
outputTerminal = outTerminal

outStrLn :: Output -> Handle -> Text -> IO ()
outStrLn Output {..} h text
    | Just tout <- outTerminal, terminalHandle tout == h = do
        void $ newLine tout text
    | otherwise = do
        T.hPutStrLn h text

outputEvent :: Output -> OutputEvent -> IO ()
outputEvent out@Output {..} = \case
    OutputMessage msg -> do
        forM_ outTerminal $ \term -> void $ newLine term msg
        forM_ outLogs $ \h -> outStrLn out h msg
        forM_ outTest $ \h -> outStrLn out h ("msg " <> msg)

    JobStarted jid -> do
        forM_ outLogs $ \h -> outStrLn out h ("Started " <> textJobId jid)
        forM_ outTest $ \h -> outStrLn out h ("job-start " <> textJobId jid)

    JobFinished jid status -> do
        forM_ outLogs $ \h -> outStrLn out h ("Finished " <> textJobId jid <> " (" <> status <> ")")
        forM_ outTest $ \h -> outStrLn out h ("job-finish " <> textJobId jid <> " " <> status)

outputFootnote :: Output -> Text -> IO OutputFootnote
outputFootnote out@Output {..} footnoteText = do
    footnoteTerminal <- forM outTerminal $ \term -> newFootnote term footnoteText
    forM_ outLogs $ \h -> outStrLn out h footnoteText
    forM_ outTest $ \h -> outStrLn out h ("note " <> footnoteText)
    return OutputFootnote {..}
