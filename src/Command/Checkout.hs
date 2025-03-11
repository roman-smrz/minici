module Command.Checkout (
    CheckoutCommand,
) where

import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

import System.Console.GetOpt

import Command
import Repo


data CheckoutCommand = CheckoutCommand CheckoutOptions (Maybe RepoName) (Maybe Text)

data CheckoutOptions = CheckoutOptions
    { coPath :: Maybe FilePath
    , coSubtree :: Maybe FilePath
    }

instance Command CheckoutCommand where
    commandName _ = "checkout"
    commandDescription _ = "Checkout (part of) a given repository"

    type CommandArguments CheckoutCommand = [ Text ]

    commandUsage _ = T.pack $ unlines $
        [ "Usage: minici checkout [<repo> [<revision>]] [<option>...]"
        ]

    type CommandOptions CheckoutCommand = CheckoutOptions
    defaultCommandOptions _ = CheckoutOptions
        { coPath = Nothing
        , coSubtree = Nothing
        }

    commandOptions _ =
        [ Option [] [ "path" ]
            (ReqArg (\val opts -> opts { coPath = Just val }) "<path>")
            "destination path"
        , Option [] [ "subtree" ]
            (ReqArg (\val opts -> opts { coSubtree = Just val }) "<path>")
            "repository subtree to checkout"
        ]

    commandInit _ co args = CheckoutCommand co
        (RepoName <$> listToMaybe args)
        (listToMaybe $ drop 1 args)
    commandExec = cmdCheckout

cmdCheckout :: CheckoutCommand -> CommandExec ()
cmdCheckout (CheckoutCommand CheckoutOptions {..} name mbrev) = do
    repo <- maybe getDefaultRepo getRepo name
    root <- getCommitTree =<< case mbrev of
        Just revision -> readCommit repo revision
        Nothing -> createWipCommit repo
    tree <- case coSubtree of
        Nothing -> return root
        Just subtree -> maybe (fail $ "subtree `" <> subtree <> "' not found in " <> maybe "current worktree" (("revision `" <>) . (<> "'") . T.unpack) mbrev) return =<<
            getSubtree subtree root
    checkoutAt tree $ maybe "." id coPath
