module Command.Checkout (
    CheckoutCommand,
) where

import Data.Text (Text)
import Data.Text qualified as T

import System.Console.GetOpt

import Command
import Repo


data CheckoutCommand = CheckoutCommand CheckoutOptions (Maybe RepoName) Text

data CheckoutOptions = CheckoutOptions
    { coPath :: Maybe FilePath
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
        }

    commandOptions _ =
        [ Option [] [ "path" ]
            (ReqArg (\val opts -> opts { coPath = Just val }) "<path>")
            "destination path"
        ]

    commandInit _ co = uncurry (CheckoutCommand co) . \case
        (name : revision : _) -> ( Just (RepoName name), revision )
        [ name ]              -> ( Just (RepoName name), "HEAD" )
        []                    -> ( Nothing, "HEAD" )
    commandExec = cmdCheckout

cmdCheckout :: CheckoutCommand -> CommandExec ()
cmdCheckout (CheckoutCommand CheckoutOptions {..} name revision) = do
    repo <- maybe getDefaultRepo getRepo name
    tree <- maybe (fail $ T.unpack $ "revision `" <> revision <> "' not found") getCommitTree =<< readCommit repo revision
    checkoutAt tree $ maybe "." id coPath
