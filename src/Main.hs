module Main (main) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.ByteString.Lazy qualified as BL
import Data.List
import Data.Proxy
import Data.Text qualified as T

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import Command
import Command.Checkout
import Command.Run
import Config
import Repo
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

options :: [ OptDescr (CmdlineOptions -> Except String CmdlineOptions) ]
options =
    [ Option [ 'h' ] [ "help" ]
        (NoArg $ \opts -> return opts { optShowHelp = True })
        "show this help and exit"
    , Option [ 'V' ] [ "version" ]
        (NoArg $ \opts -> return opts { optShowVersion = True })
        "show version and exit"
    , Option [ 'j' ] [ "jobs" ]
        (ReqArg (\num opts -> return opts { optCommon = (optCommon opts) { optJobs = read num }}) "<num>")
        ("number of jobs to run simultaneously (default " <> show (optJobs defaultCommonOptions) <> ")")
    , Option [] [ "repo" ]
        (ReqArg (\value opts ->
            case span (/= ':') value of
                ( repo, ':' : path ) -> return opts
                    { optCommon = (optCommon opts)
                        { optRepo = DeclaredRepo (RepoName $ T.pack repo) path : optRepo (optCommon opts)
                        }
                    }
                _ -> throwError $ "--repo: invalid value `" <> value <> "'"
        ) "<repo>:<path>")
        ("override or declare repo path")
    ]

data SomeCommandType = forall c. Command c => SC (Proxy c)

commands :: [ SomeCommandType ]
commands =
    [ SC $ Proxy @RunCommand
    , SC $ Proxy @CheckoutCommand
    ]

lookupCommand :: String -> Maybe SomeCommandType
lookupCommand name = find p commands
  where
    p (SC cmd) = commandName cmd == name

main :: IO ()
main = do
    args <- getArgs
    let ( mbConfigPath, args' ) = case args of
            (path : rest)
                | any isPathSeparator path -> ( Just path, rest )
            _ -> ( Nothing, args )

    (opts, cmdargs) <- case getOpt RequireOrder options args' of
        (os, cmdargs, []) -> do
            let merge :: ([String], CmdlineOptions) -> (CmdlineOptions -> Except String CmdlineOptions) -> ([String], CmdlineOptions)
                merge ( errs, o ) f = case runExcept $ f o of
                    Left err -> ( err : errs, o )
                    Right o' -> ( errs, o' )

            case foldl merge ( [], defaultCmdlineOptions ) os of
                ( [], opts ) -> return ( opts , cmdargs )
                ( errs, _ ) -> do
                    hPutStrLn stderr $ unlines (reverse errs) <> "Try `minici --help' for more information."
                    exitFailure
        (_, _, errs) -> do
            hPutStrLn stderr $ concat errs <> "Try `minici --help' for more information."
            exitFailure

    when (optShowHelp opts) $ do
        let header = "Usage: minici [<job-file>] [<option>...] <command> [<args>]\n\nCommon options are:"
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

    ( configPath, cmdargs' ) <- case ( mbConfigPath, cmdargs ) of
        ( Just path, _ )
            -> return ( Just path, cmdargs )
        ( _, path : rest )
            | any isPathSeparator path
            -> return ( Just path, rest )
        _ -> ( , cmdargs ) <$> findConfig

    ( ncmd, cargs ) <- case cmdargs' of
        [] -> return ( head commands, [] )

        (cname : cargs)
            | Just nc <- lookupCommand cname -> return (nc, cargs)
            | otherwise -> do
                hPutStr stderr $ unlines
                    [ "Unknown command `" <> cname <> "'."
                    , "Try `minici --help' for more information."
                    ]
                exitFailure

    runSomeCommand configPath (optCommon opts) ncmd cargs

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

runSomeCommand :: Maybe FilePath -> CommonOptions -> SomeCommandType -> [ String ] -> IO ()
runSomeCommand ciConfigPath ciOptions (SC tproxy) args = do
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

    ciConfig <- case ciConfigPath of
        Just path -> parseConfig <$> BL.readFile path
        Nothing -> return $ Left "no job file found"

    let cmd = commandInit tproxy (fcoSpecific opts) cmdargs
    let CommandExec exec = commandExec cmd

    ciContainingRepo <- maybe (return Nothing) (openRepo . takeDirectory) ciConfigPath

    let openDeclaredRepo dir decl = do
            let path = dir </> repoPath decl
            openRepo path >>= \case
                Just repo -> return ( repoName decl, repo )
                Nothing -> do
                    absPath <- makeAbsolute path
                    hPutStrLn stderr $ "Failed to open repo `" <> showRepoName (repoName decl) <> "' at " <> repoPath decl <> " (" <> absPath <> ")"
                    exitFailure

    cmdlineRepos <- forM (optRepo ciOptions) (openDeclaredRepo "")
    configRepos <- case ( ciConfigPath, ciConfig ) of
        ( Just path, Right config ) ->
            forM (configRepos config) $ \decl -> do
                case lookup (repoName decl) cmdlineRepos of
                    Just repo -> return ( repoName decl, repo )
                    Nothing -> openDeclaredRepo (takeDirectory path) decl
        _ -> return []

    let ciOtherRepos = configRepos ++ cmdlineRepos

    ciTerminalOutput <- initTerminalOutput
    flip runReaderT CommandInput {..} exec
