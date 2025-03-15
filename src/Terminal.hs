module Terminal (
    TerminalOutput,
    TerminalLine,
    TerminalFootnote(..),
    initTerminalOutput,
    newLine,
    redrawLine,
    newFootnote,
    terminalBlinkStatus,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import Data.Function
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.IO


data TerminalOutput = TerminalOutput
    { outNumLines :: MVar Int
    , outNextFootnote :: MVar Int
    , outBlinkVar :: TVar Bool
    }

instance Eq TerminalOutput where
    (==) = (==) `on` outNumLines

data TerminalLine = TerminalLine
    { lineOutput :: TerminalOutput
    , lineNum :: Int
    }
    deriving (Eq)

data TerminalFootnote = TerminalFootnote
    { footnoteLine :: TerminalLine
    , footnoteNumber :: Int
    , footnoteText :: Text
    }
    deriving (Eq)

initTerminalOutput :: IO TerminalOutput
initTerminalOutput = do
    outNumLines <- newMVar 0
    outNextFootnote <- newMVar 1
    outBlinkVar <- newTVarIO False
    void $ forkIO $ forever $ do
        threadDelay 500000
        atomically $ writeTVar outBlinkVar . not =<< readTVar outBlinkVar
    return TerminalOutput {..}

newLine :: TerminalOutput -> Text -> IO TerminalLine
newLine lineOutput@TerminalOutput {..} text = do
    modifyMVar outNumLines $ \lineNum -> do
        T.putStrLn text
        hFlush stdout
        return ( lineNum + 1, TerminalLine {..} )

redrawLine :: TerminalLine -> Text -> IO ()
redrawLine TerminalLine {..} text = do
    let TerminalOutput {..} = lineOutput
    withMVar outNumLines $ \total -> do
        let moveBy = total - lineNum
        T.putStr $ "\ESC[s\ESC[" <> T.pack (show moveBy) <> "F" <> text <> "\ESC[u"
        hFlush stdout

newFootnote :: TerminalOutput -> Text -> IO TerminalFootnote
newFootnote tout@TerminalOutput {..} footnoteText = do
    modifyMVar outNextFootnote $ \footnoteNumber -> do
        footnoteLine <- newLine tout $ "[" <> T.pack (show footnoteNumber) <> "] " <> footnoteText
        hFlush stdout
        return ( footnoteNumber + 1, TerminalFootnote {..} )

terminalBlinkStatus :: TerminalOutput -> STM Bool
terminalBlinkStatus TerminalOutput {..} = readTVar outBlinkVar
