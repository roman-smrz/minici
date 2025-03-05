module Command.Checkout (
    CheckoutCommand,
) where

import Data.Text (Text)
import Data.Text qualified as T

import Command
import Repo


data CheckoutCommand = CheckoutCommand (Maybe RepoName) Text

instance Command CheckoutCommand where
    commandName _ = "checkout"
    commandDescription _ = "Checkout (part of) a given repository"

    type CommandArguments CheckoutCommand = [ Text ]

    commandUsage _ = T.pack $ unlines $
        [ "Usage: minici checkout [<repo> [<revision>]] [<option>...]"
        ]

    commandInit _ _ = \case
        (name : revision : _) -> CheckoutCommand (Just (RepoName name)) revision
        [ name ] -> CheckoutCommand (Just (RepoName name)) "HEAD"
        [] -> CheckoutCommand Nothing "HEAD"
    commandExec = cmdCheckout

cmdCheckout :: CheckoutCommand -> CommandExec ()
cmdCheckout (CheckoutCommand name revision) = do
    repo <- maybe getDefaultRepo getRepo name
    commit <- maybe (fail $ T.unpack $ "revision `" <> revision <> "' not found") return =<< readCommit repo revision
    checkoutAt commit "."
