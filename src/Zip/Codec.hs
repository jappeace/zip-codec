{-# LANGUAGE DeriveAnyClass #-}

module Zip.Codec
  ( CodecErrors(..)
  , withArchive
  )
where

import Control.Exception
import           System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime)
import Zip.Codec.End
import Zip.Codec.FileHeader
import           Prelude hiding (readFile, zip)
import           Control.Applicative ((<$>))
import           Control.Monad (foldM, forM_)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B (empty)
import           Data.List (find)
import           Data.Maybe (fromMaybe)
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Word (Word32)
import           System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime)
import           System.FilePath ((</>), dropDrive, takeDirectory)
import           System.IO (Handle, IOMode(..), SeekMode(..), hClose, hSeek, hTell, openFile, withFile)

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (StateT, evalStateT, get, gets, modify, put)
import           Control.Monad.Trans.Resource (ResourceT, MonadResource, runResourceT)
import           Data.Conduit (ConduitT, ($$), (.|), (=$))
import qualified Data.Conduit.Binary as CB (isolate, sinkFile, sinkHandle, sourceFile, sourceIOHandle)
import qualified Data.Conduit.List as CL (map)
import qualified Data.Conduit.Internal as CI (zipSinks)
import           Data.Conduit.Zlib (WindowBits(..), compress, decompress)
import Data.Map(Map)
import Zip.Codec.CentralDirectory
import Control.Monad.Trans.Except
import Data.Bifunctor
import Control.Monad.Primitive
import Control.Monad.Catch

data CodecErrors = FailedEndReading String
                 | FailedCentralDirectoryReading CenteralDirErrors

-- | this opens up a file from the filesystem
withArchive :: (MonadThrow m, PrimMonad m, MonadResource m) => FilePath -> IO (Either CodecErrors (Map FilePath (ConduitT () ByteString m ())))
withArchive zipPath =
    withFile zipPath ReadMode $ \handle -> runExceptT $ do
      end <- except . first FailedEndReading =<< liftIO (readEnd handle)
      central <- except . first FailedCentralDirectoryReading =<< liftIO (readCentralDirectory handle end)
      pure $
        sourceFile zipPath <$> cdFileHeaders central

data SourceFileError = OffsetError String
  deriving stock Show
  deriving anyclass Exception

sourceFile :: (MonadThrow m, PrimMonad m, MonadResource m) => FilePath -> FileHeader -> ConduitT () ByteString m ()
sourceFile zipPath fileHeader =
    source .| CB.isolate (fromIntegral $ fhCompressedSize fileHeader)
           .| decomp
  where
    source = CB.sourceIOHandle $ do
        h      <- openFile zipPath ReadMode
        offset' <- calculateFileDataOffset h fileHeader
        case offset' of
          Left err -> throwIO $ OffsetError err
          Right offset -> do
            hSeek h AbsoluteSeek offset
            return h

    decomp =
        case fhCompressionMethod fileHeader of
          NoCompression -> CL.map id
          Deflate       -> decompress $ WindowBits (-15)
