module Terminal (
    TerminalOutput,
    TerminalLine,
    initTerminalOutput,
    newLine,
    redrawLine,
) where

import Control.Concurrent

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.IO


data TerminalOutput = TerminalOutput
    { outNumLines :: MVar Int
    }

data TerminalLine = TerminalLine
    { lineOutput :: TerminalOutput
    , lineNum :: Int
    }

initTerminalOutput :: IO TerminalOutput
initTerminalOutput = do
    outNumLines <- newMVar 0
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
