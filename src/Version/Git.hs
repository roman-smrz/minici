module Version.Git (
    tGitVersion,
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Compat

import System.Directory
import System.Exit
import System.Process

tGitVersion :: SpliceQ (Maybe String)
tGitVersion = unsafeSpliceCoerce $ do
    let git args = do
            (ExitSuccess, out, _) <- readCreateProcessWithExitCode
                (proc "git" $ [ "--git-dir=./.git", "--work-tree=." ] ++ args) ""
            return $ lines out

    mbver <- runIO $ do
        doesPathExist "./.git" >>= \case
            False -> return Nothing
            True -> do
                desc:_ <- git [ "describe", "--always", "--dirty= (dirty)" ]
                files <- git [ "ls-files" ]
                return $ Just (desc, files)

    case mbver of
        Just (_, files) -> mapM_ addDependentFile files
        Nothing -> return ()

    lift (fst <$> mbver :: Maybe String)
