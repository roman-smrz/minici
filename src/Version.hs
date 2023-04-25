{-# LANGUAGE TemplateHaskell #-}

module Version (
    versionLine,
) where

import Paths_minici (version)
import Data.Version (showVersion)
import Version.Git

{-# NOINLINE versionLine #-}
versionLine :: String
versionLine = do
    let ver = case $$tGitVersion of
            Just gver
                | 'v':v <- gver, not $ all (`elem` ('.': ['0'..'9'])) v
                -> "git " <> gver
            _   -> "version " <> showVersion version
     in "MiniCI " <> ver
