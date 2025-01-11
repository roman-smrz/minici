module Main (main) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.ByteString.Lazy qualified as BL
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
import Terminal
import Version

data CmdlineOptions = CmdlineOptions
    { optShowHelp :: Bool
    , optShowVersion :: Bool
    , optCommon :: CommonOptions
    }

defaultCmdlineOptions :: CmdlineOptions
defaultCmdlineOptions = CmdlineOptions
    { optShowHelp = False
    , optShowVersion = False
    , optCommon = defaultCommonOptions
    }

options :: [OptDescr (CmdlineOptions -> CmdlineOptions)]
options =
    [ Option ['h'] ["help"]
        (NoArg $ \opts -> opts { optShowHelp = True })
        "show this help and exit"
    , Option ['V'] ["version"]
        (NoArg $ \opts -> opts { optShowVersion = True })
        "show version and exit"
    , Option ['j'] ["jobs"]
        (ReqArg (\num opts -> opts { optCommon = (optCommon opts) { optJobs = read num }}) "<num>")
        ("number of jobs to run simultaneously (default " <> show (optJobs defaultCommonOptions) <> ")")
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
        let header = "Usage: minici [<option>...] <command> [<args>]\n\nCommon options are:"
            commandDesc (SC proxy) = "  " <> padCommand (commandName proxy) <> commandDescription proxy

            padTo n str = str <> replicate (n - length str) ' '
            padCommand = padTo (maxCommandNameLength + 3)
            commandNameLength (SC proxy) = length $ commandName proxy
            maxCommandNameLength = maximum $ map commandNameLength commands

        putStr $ usageInfo header options <> unlines (
            [ ""
            , "Available commands:"
            ] ++ map commandDesc commands
          )
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

    runSomeCommand (optCommon opts) ncmd cargs

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

runSomeCommand :: CommonOptions -> SomeCommandType -> [ String ] -> IO ()
runSomeCommand ciOptions (SC tproxy) args = do
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
    BL.readFile configPath >>= return . parseConfig >>= \case
        Left err -> do
            putStr err
            exitFailure
        Right ciConfig -> do
            let cmd = commandInit tproxy (fcoSpecific opts) cmdargs
            let CommandExec exec = commandExec cmd
            ciTerminalOutput <- initTerminalOutput
            flip runReaderT CommandInput {..} exec
