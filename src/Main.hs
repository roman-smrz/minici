module Main (main) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.List
import Data.Proxy
import Data.Text qualified as T

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
    (opts, cmdargs) <- case getOpt RequireOrder options args of
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

data FullCommandOptions c = FullCommandOptions
    { fcoSpecific :: CommandOptions c
    , fcoShowHelp :: Bool
    }

defaultFullOptions :: Command c => proxy c -> FullCommandOptions c
defaultFullOptions proxy = FullCommandOptions
    { fcoSpecific = defaultCommandOptions proxy
    , fcoShowHelp = False
    }

fullCommandOptions :: Command c => proxy c -> [ OptDescr (FullCommandOptions c -> FullCommandOptions c) ]
fullCommandOptions proxy =
    map (fmap $ \f fco -> fco { fcoSpecific = f (fcoSpecific fco) } ) (commandOptions proxy)
    ++
    [ Option [ 'h' ] [ "help" ]
        (NoArg $ \opts -> opts { fcoShowHelp = True })
        "show this help and exit"
    ]

runSomeCommand :: SomeCommandType -> [ String ] -> IO ()
runSomeCommand (SC tproxy) args = do
    let exitWithErrors errs = do
            hPutStrLn stderr $ concat errs <> "Try `minici " <> commandName tproxy <> " --help' for more information."
            exitFailure

    (opts, cmdargs) <- case getOpt Permute (fullCommandOptions tproxy) args of
        (o, strargs, []) -> case runExcept $ argsFromStrings strargs of
            Left err -> exitWithErrors [ err <> "\n" ]
            Right cmdargs -> do
                let fullOptions = foldl (flip id) (defaultFullOptions tproxy) o
                return (fullOptions, cmdargs)
        (_, _, errs) -> exitWithErrors errs

    when (fcoShowHelp opts) $ do
        putStr $ usageInfo (T.unpack $ commandUsage tproxy) (fullCommandOptions tproxy)
        exitSuccess

    Just configPath <- findConfig
    config <- parseConfig configPath
    let cmd = commandInit tproxy (fcoSpecific opts) cmdargs
    let CommandExec exec = commandExec cmd
    flip runReaderT config exec
