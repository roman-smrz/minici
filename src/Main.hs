module Main (main) where

import Control.Monad
import Control.Monad.Reader

import Data.List
import Data.Proxy

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Command
import Command.Run
import Config
import Version

data CmdlineOptions = CmdlineOptions
    { optShowHelp :: Bool
    , optShowVersion :: Bool
    }

defaultCmdlineOptions :: CmdlineOptions
defaultCmdlineOptions = CmdlineOptions
    { optShowHelp = False
    , optShowVersion = False
    }

options :: [OptDescr (CmdlineOptions -> CmdlineOptions)]
options =
    [ Option ['h'] ["help"]
        (NoArg $ \opts -> opts { optShowHelp = True })
        "show this help and exit"
    , Option ['V'] ["version"]
        (NoArg $ \opts -> opts { optShowVersion = True })
        "show version and exit"
    ]

data SomeCommandType = forall c. Command c => SC (Proxy c)

commands :: [ SomeCommandType ]
commands =
    [ SC $ Proxy @RunCommand
    ]

lookupCommand :: String -> Maybe SomeCommandType
lookupCommand name = find p commands
  where
    p (SC cmd) = commandName cmd == name

main :: IO ()
main = do
    args <- getArgs
    (opts, cmdargs) <- case getOpt Permute options args of
        (o, cmdargs, []) -> return (foldl (flip id) defaultCmdlineOptions o, cmdargs)
        (_, _, errs) -> do
            hPutStrLn stderr $ concat errs <> "Try `minici --help' for more information."
            exitFailure

    when (optShowHelp opts) $ do
        let header = "Usage: minici [<option>...] <command> [<args>]"
        putStr $ usageInfo header options
        exitSuccess

    when (optShowVersion opts) $ do
        putStrLn versionLine
        exitSuccess

    (ncmd, cargs) <- case cmdargs of
        [] -> return (head commands, [])
        (cname : cargs)
            | Just nc <- lookupCommand cname -> return (nc, cargs)
            | otherwise -> do
                hPutStr stderr $ unlines
                    [ "Unknown command `" <> cname <> "'."
                    , "Try `minici --help' for more information."
                    ]
                exitFailure

    runSomeCommand ncmd cargs

runSomeCommand :: SomeCommandType -> [ String ] -> IO ()
runSomeCommand (SC tproxy) _ = do
    Just configPath <- findConfig
    config <- parseConfig configPath
    let cmd = commandInit tproxy
    let CommandExec exec = commandExec cmd
    flip runReaderT config exec
