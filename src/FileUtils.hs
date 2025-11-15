module FileUtils where

import Control.Monad
import Control.Monad.Catch

import Data.ByteString (useAsCString)
import Data.Text qualified as T
import Data.Text.Encoding

import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr

import System.Directory
import System.FilePath
import System.Posix.IO.ByteString
import System.Posix.Types


-- As of directory-1.3.9 and file-io-0.1.5, the provided copyFile creates a
-- temporary file without O_CLOEXEC, sometimes leaving the write descriptor
-- open in child processes.
safeCopyFile :: FilePath -> FilePath -> IO ()
safeCopyFile from to = do
    allocaBytes (fromIntegral bufferSize) $ \buf ->
        useAsCString (encodeUtf8 $ T.pack from) $ \cfrom ->
        useAsCString (encodeUtf8 $ T.pack to) $ \cto ->
        bracket (throwErrnoPathIfMinus1 "open" from $ c_fd_open_read cfrom) closeFd $ \fromFd ->
        bracket (throwErrnoPathIfMinus1 "open" to $ c_fd_create_write cto fromFd) closeFd $ \toFd -> do
            let goRead = do
                    count <- throwErrnoIfMinus1Retry ("read " <> from) $ fdReadBuf fromFd buf bufferSize
                    when (count > 0) $ do
                        goWrite count 0
                goWrite count written
                    | written < count = do
                        written' <- throwErrnoIfMinus1Retry ("write " <> to) $
                            fdWriteBuf toFd (buf `plusPtr` fromIntegral written) (count - written)
                        goWrite count (written + written')
                    | otherwise = do
                        goRead
            goRead
  where
    bufferSize = 131072

-- Custom open(2) wrappers using O_CLOEXEC. The `cloexec` in `OpenFileFlags` is
-- available only since unix-2.8.0.0
foreign import ccall "minici_fd_open_read" c_fd_open_read :: CString -> IO Fd
foreign import ccall "minici_fd_create_write" c_fd_create_write :: CString -> Fd -> IO Fd


copyRecursive :: FilePath -> FilePath -> IO ()
copyRecursive from to = do
    doesDirectoryExist from >>= \case
        False -> do
            safeCopyFile from to
        True -> do
            createDirectory to
            content <- listDirectory from
            forM_ content $ \name -> do
                copyRecursive  (from </> name) (to </> name)

copyRecursiveForce :: FilePath -> FilePath -> IO ()
copyRecursiveForce from to = do
    doesDirectoryExist to >>= \case
        False -> return ()
        True  -> removeDirectoryRecursive to
    copyRecursive from to
