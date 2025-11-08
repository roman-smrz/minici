module Main (main) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.ByteString.Lazy qualified as BL
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Proxy
import Data.Text qualified as T

import System.Console.ANSI
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import Command
import Command.Checkout
import Command.Extract
import Command.JobId
import Command.Log
import Command.Run
import Command.Shell
import Command.Subtree
import Config
import Output
import Repo
import Version

data CmdlineOptions = CmdlineOptions
    { optShowHelp :: Bool
    , optShowVersion :: Bool
    , optCommon :: CommonOptions
    , optStorage :: Maybe FilePath
    , optOutput :: Maybe [ OutputType ]
    }

defaultCmdlineOptions :: CmdlineOptions
defaultCmdlineOptions = CmdlineOptions
    { optShowHelp = False
    , optShowVersion = False
    , optCommon = defaultCommonOptions
    , optStorage = Nothing
    , optOutput = Nothing
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
                        { optRepo = ( RepoName $ T.pack repo, path ) : optRepo (optCommon opts)
                        }
                    }
                _ -> throwError $ "--repo: invalid value ‘" <> value <> "’"
        ) "<repo>:<path>")
        ("override or declare repo path")
    , Option [] [ "storage" ]
        (ReqArg (\value opts -> return opts { optStorage = Just value }) "<path>")
        "set storage path"
    , Option [] [ "terminal-output" ]
        (NoArg $ \opts -> return opts { optOutput = Just $ TerminalOutput : fromMaybe [] (optOutput opts) })
        "use terminal-style output (default if standard output is terminal)"
    , Option [] [ "log-output" ]
        (OptArg (\value opts -> return opts { optOutput = Just $ LogOutput (fromMaybe "-" value) : fromMaybe [] (optOutput opts) }) "<path>")
        "use log-style output to <path> or standard output"
    , Option [] [ "test-output" ]
        (OptArg (\value opts -> return opts { optOutput = Just $ TestOutput (fromMaybe "-" value) : fromMaybe [] (optOutput opts) }) "<path>")
        "use test-style output to <path> or standard output"
    ]

data SomeCommandType = forall c. Command c => SC (Proxy c)

commands :: NE.NonEmpty SomeCommandType
commands =
    ( SC $ Proxy @RunCommand) NE.:|
    [ SC $ Proxy @CheckoutCommand
    , SC $ Proxy @ExtractCommand
    , SC $ Proxy @JobIdCommand
    , SC $ Proxy @LogCommand
    , SC $ Proxy @ShellCommand
    , SC $ Proxy @SubtreeCommand
    ]

lookupCommand :: String -> Maybe SomeCommandType
lookupCommand name = find p commands
  where
    p (SC cmd) = commandName cmd == name

main :: IO ()
main = do
    args <- getArgs
    let isPathArgument path = maybe False (/= '-') (listToMaybe path) && any isPathSeparator path
    let ( mbRootPath, args' ) = case args of
            (path : rest)
                | isPathArgument path -> ( Just path, rest )
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
                    hPutStrLn stderr $ unlines (reverse errs) <> "Try ‘minici --help’ for more information."
                    exitFailure
        (_, _, errs) -> do
            hPutStrLn stderr $ concat errs <> "Try ‘minici --help’ for more information."
            exitFailure

    when (optShowHelp opts) $ do
        let header = "Usage: minici [<job-file>] [<option>...] <command> [<args>]\n\nCommon options are:"
            commandDesc (SC proxy) = "  " <> padCommand (commandName proxy) <> commandDescription proxy

            padTo n str = str <> replicate (n - length str) ' '
            padCommand = padTo (maxCommandNameLength + 3)
            commandNameLength (SC proxy) = length $ commandName proxy
            maxCommandNameLength = maximum $ fmap commandNameLength commands

        putStr $ usageInfo header options <> unlines (
            [ ""
            , "Available commands:"
            ] ++ map commandDesc (NE.toList commands)
          )
        exitSuccess

    when (optShowVersion opts) $ do
        putStrLn versionLine
        exitSuccess

    ( rootPath, cmdargs' ) <- case ( mbRootPath, cmdargs ) of
        ( Just path, _ )
            -> return ( Just path, cmdargs )
        ( _, path : rest )
            | isPathArgument path
            -> return ( Just path, rest )
        _   -> return ( Nothing , cmdargs )

    ( ncmd, cargs ) <- case cmdargs' of
        [] -> return ( NE.head commands, [] )

        (cname : cargs)
            | Just nc <- lookupCommand cname -> return (nc, cargs)
            | otherwise -> do
                hPutStr stderr $ unlines
                    [ "Unknown command ‘" <> cname <> "’."
                    , "Try ‘minici --help’ for more information."
                    ]
                exitFailure

    runSomeCommand rootPath opts ncmd cargs

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

runSomeCommand :: Maybe FilePath -> CmdlineOptions -> SomeCommandType -> [ String ] -> IO ()
runSomeCommand rootPath gopts (SC tproxy) args = do
    let reportFailure err = hPutStrLn stderr err >> exitFailure
    ( ciRootPath, ciJobRoot ) <- case rootPath of
        Just path -> do
            doesFileExist path >>= \case
                True -> BL.readFile path >>= return . parseConfig >>= \case
                    Right config -> return ( path, JobRootConfig config )
                    Left err -> reportFailure $ "Failed to parse job file ‘" <> path <> "’:" <> err
                False -> doesDirectoryExist path >>= \case
                    True -> openRepo path >>= \case
                        Just repo -> return ( path, JobRootRepo repo )
                        Nothing -> reportFailure $ "Failed to open repository ‘" <> path <> "’"
                    False -> reportFailure $ "File or directory ‘" <> path <> "’ not found"
        Nothing -> do
            openRepo "." >>= \case
                Just repo -> return ( ".", JobRootRepo repo )
                Nothing -> findConfig >>= \case
                    Just path -> BL.readFile path >>= return . parseConfig >>= \case
                        Right config -> return ( path, JobRootConfig config )
                        Left err -> reportFailure $ "Failed to parse job file ‘" <> path <> "’:" <> err
                    Nothing -> reportFailure $ "No job file or repository found"

    let storageFileName = ".minici"
        ciStorageDir = case ( optStorage gopts, ciRootPath, ciJobRoot ) of
            ( Just path, _   , _                ) -> path
            ( Nothing  , path, JobRootConfig {} ) -> takeDirectory path </> storageFileName
            ( Nothing  , _   , JobRootRepo repo ) -> getRepoWorkDir repo </> storageFileName

    let ciOptions = optCommon gopts
    let exitWithErrors errs = do
            hPutStrLn stderr $ concat errs <> "Try ‘minici " <> commandName tproxy <> " --help’ for more information."
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

    let cmd = commandInit tproxy (fcoSpecific opts) cmdargs
    let CommandExec exec = commandExec cmd

    ciContainingRepo <- case ciJobRoot of
        JobRootRepo repo -> return (Just repo)
        JobRootConfig _  -> openRepo $ takeDirectory ciRootPath

    let openDeclaredRepo dir ( name, dpath ) = do
            let path = dir </> dpath
            openRepo path >>= \case
                Just repo -> return ( name, repo )
                Nothing -> do
                    absPath <- makeAbsolute path
                    hPutStrLn stderr $ "Failed to open repo ‘" <> showRepoName name <> "’ at " <> dpath <> " (" <> absPath <> ")"
                    exitFailure

    cmdlineRepos <- forM (optRepo ciOptions) (openDeclaredRepo "")
    configRepos <- case ciJobRoot of
        JobRootConfig config ->
            forM (configRepos config) $ \decl -> do
                case lookup (repoName decl) cmdlineRepos of
                    Just repo -> return ( repoName decl, repo )
                    Nothing
                        | Just path <- repoPath decl
                        -> openDeclaredRepo (takeDirectory ciRootPath) ( repoName decl, path )

                        | otherwise
                        -> do
                            hPutStrLn stderr $ "No path defined for repo ‘" <> showRepoName (repoName decl) <> "’"
                            exitFailure
        _ -> return []

    let ciOtherRepos = configRepos ++ cmdlineRepos

    outputTypes <- case optOutput gopts of
        Just types -> return types
        Nothing -> hSupportsANSI stdout >>= return . \case
            True -> [ TerminalOutput ]
            False -> [ LogOutput "-" ]
    withOutput outputTypes $ \ciOutput -> do
        flip runReaderT CommandInput {..} exec
