module Command.Subtree (
    SubtreeCommand,
) where

import Data.Text (Text)
import Data.Text qualified as T

import Command
import Output
import Repo


data SubtreeCommand = SubtreeCommand SubtreeOptions [ Text ]

data SubtreeOptions = SubtreeOptions

instance Command SubtreeCommand where
    commandName _ = "subtree"
    commandDescription _ = "Resolve subdirectory of given repo tree"

    type CommandArguments SubtreeCommand = [ Text ]

    commandUsage _ = T.pack $ unlines $
        [ "Usage: minici subtree <tree> <path>"
        ]

    type CommandOptions SubtreeCommand = SubtreeOptions
    defaultCommandOptions _ = SubtreeOptions

    commandInit _ opts = SubtreeCommand opts
    commandExec = cmdSubtree


cmdSubtree :: SubtreeCommand -> CommandExec ()
cmdSubtree (SubtreeCommand SubtreeOptions args) = do
    [ treeParam, path ] <- return args
    out <- getOutput
    repo <- getDefaultRepo

    let ( tree, subdir ) =
            case T.splitOn "(" treeParam of
                (t : param : _) -> ( t, T.unpack $ T.takeWhile (/= ')') param )
                _ -> ( treeParam, "" )

    subtree <- getSubtree Nothing (T.unpack path) =<< readTree repo subdir tree
    outputMessage out $ textTreeId $ treeId subtree
