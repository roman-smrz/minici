module Terminal (
    TerminalOutput,
    TerminalLine,
    TerminalFootnote(..),
    initTerminalOutput,
    newLine,
    redrawLine,
    newFootnote,
    terminalHandle,
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
    { outHandle :: Handle
    , outNumLines :: MVar Int
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
    { tfLine :: TerminalLine
    , tfNumber :: Int
    }
    deriving (Eq)

initTerminalOutput :: IO TerminalOutput
initTerminalOutput = do
    outHandle <- return stdout
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
        hFlush outHandle
        return ( lineNum + 1, TerminalLine {..} )

redrawLine :: TerminalLine -> Text -> IO ()
redrawLine TerminalLine {..} text = do
    let TerminalOutput {..} = lineOutput
    withMVar outNumLines $ \total -> do
        let moveBy = total - lineNum
        T.putStr $ "\ESC[s\ESC[" <> T.pack (show moveBy) <> "F" <> text <> "\ESC[u"
        hFlush outHandle

newFootnote :: TerminalOutput -> Text -> IO TerminalFootnote
newFootnote tout@TerminalOutput {..} text = do
    modifyMVar outNextFootnote $ \tfNumber -> do
        tfLine <- newLine tout $ "[" <> T.pack (show tfNumber) <> "] " <> text
        hFlush outHandle
        return ( tfNumber + 1, TerminalFootnote {..} )

terminalHandle :: TerminalOutput -> Handle
terminalHandle = outHandle

terminalBlinkStatus :: TerminalOutput -> STM Bool
terminalBlinkStatus TerminalOutput {..} = readTVar outBlinkVar
