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
    { coDestination :: Maybe FilePath
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
        { coDestination = Nothing
        , coSubtree = Nothing
        }

    commandOptions _ =
        [ Option [] [ "dest" ]
            (ReqArg (\val opts -> opts { coDestination = Just val }) "<path>")
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
    mbCommit <- sequence $ fmap (readCommit repo) mbrev
    root <- getCommitTree =<< maybe (createWipCommit repo) return mbCommit
    tree <- maybe return (getSubtree mbCommit) coSubtree $ root
    checkoutAt tree $ maybe "." id coDestination
