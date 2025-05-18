module Output (
    Output,
    OutputType(..),
    OutputEvent(..),
    OutputFootnote(..),

    withOutput,
    outputTerminal,
    outputMessage,
    outputEvent,
    outputFootnote,
) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Text (Text)
import Data.Text.IO qualified as T

import System.IO

import Job.Types
import Terminal


data Output = Output
    { outLock :: MVar ()
    , outTerminal :: Maybe TerminalOutput
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
    | TestMessage Text
    | LogMessage Text
    | JobStarted JobId
    | JobFinished JobId Text

data OutputFootnote = OutputFootnote
    { footnoteText :: Text
    , footnoteTerminal :: Maybe TerminalFootnote
    }
    deriving (Eq)


withOutput :: [ OutputType ] -> (Output -> IO a) -> IO a
withOutput types inner = do
    lock <- newMVar ()
    go types (Output lock Nothing [] [])
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
        withMVar outLock $ \_ -> do
            T.hPutStrLn h text

outputMessage :: MonadIO m => Output -> Text -> m ()
outputMessage out msg = outputEvent out (OutputMessage msg)

outputEvent :: MonadIO m => Output -> OutputEvent -> m ()
outputEvent out@Output {..} = liftIO . \case
    OutputMessage msg -> do
        forM_ outTerminal $ \term -> void $ newLine term msg
        forM_ outLogs $ \h -> outStrLn out h msg
        forM_ outTest $ \h -> outStrLn out h ("msg " <> msg)

    TestMessage msg -> do
        forM_ outTest $ \h -> outStrLn out h msg

    LogMessage msg -> do
        forM_ outLogs $ \h -> outStrLn out h msg

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
