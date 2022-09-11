{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Zip.Codec
  (
    -- * reading
    CodecErrors(..)
  , readZipFile
  , FileContent(..)
    -- * writing
  , writeZipFile
  , defOptions
  , fromFileHeader
  , appendBytestring
  )
where

import Zip.Codec.DataDescriptor
import Data.Text(Text)
import Control.Exception
import Zip.Codec.End
import Zip.Codec.FileHeader
import Data.Void(Void)
import qualified Data.Conduit.Internal as CI (zipSinks)
import           Prelude hiding (readFile, zip)
import           Data.ByteString (ByteString)
import           Data.Time (UTCTime(..))
import           Data.Word
import           System.IO (Handle, IOMode(..), SeekMode(..), hSeek, openFile, withFile)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Conduit (ConduitT, (.|), yield)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Zlib (WindowBits(..), decompress)
import Data.Map(Map)
import Zip.Codec.CentralDirectory
import Control.Monad.Trans.Except
import Data.Bifunctor
import Control.Monad.Primitive
import Control.Monad.Catch

data CodecErrors = FailedEndReading String
                 | FailedCentralDirectoryReading CenteralDirErrors
                 deriving (Show, Exception)

data FileContent m = MkFileContent
  { fcFileHeader :: FileInZipOptions -- ^ simplified representation of FileHeader
  , fcFileContents :: ConduitT () ByteString m ()
  }

-- | this opens up a file from the filesystem
--   it provides a map of internal file with a conduit to the data
readZipFile :: (MonadThrow m, PrimMonad m, MonadResource m) => FilePath -> IO (Either CodecErrors (Map FilePath (FileContent m)))
readZipFile zipPath =
    withFile zipPath ReadMode $ \handle' -> runExceptT $ do
      end <- except . first FailedEndReading =<< liftIO (readEnd handle')
      central <- except . first FailedCentralDirectoryReading =<< liftIO (readCentralDirectory handle' end)
      pure $
        (\header -> ( MkFileContent
                  { fcFileHeader = fromFileHeader header
                  , fcFileContents = sourceFile zipPath header
                  })) <$> cdFileHeaders central

data FileInZipOptions = MkFileInZipOptions {
    fizCompression  :: CompressionMethod
  , fizModification :: UTCTime
  , fizBitflag      :: Word16
  , fizExtraField   :: ByteString
  , fizComment      :: Text
  } deriving (Show, Eq)

defOptions :: Monad m => FileContent m
defOptions = MkFileContent
    { fcFileHeader = MkFileInZipOptions
      { fizCompression  = Deflate
      , fizModification = UTCTime { utctDay = toEnum 0, utctDayTime = 0}
      , fizBitflag      = 2  -- max compression for deflate compression method
      , fizExtraField   = mempty
      , fizComment      = mempty
      }
    , fcFileContents     = yield mempty
    }

-- | appends a bytestring to the content conduit
appendBytestring :: Monad m => ByteString -> FileContent m -> FileContent m
appendBytestring bs opts = opts{ fcFileContents = fcFileContents opts <> yield bs }


fromFileHeader :: FileHeader -> FileInZipOptions
fromFileHeader FileHeader{..} =
  MkFileInZipOptions
  { fizCompression  = fhCompressionMethod
  , fizModification = fhLastModified
  , fizBitflag      = fhBitFlag
  , fizExtraField   = fhExtraField
  , fizComment      = fhFileComment
  }

-- shouldn't this return a map of sinks instead?
-- I'm not sure how finilzation works then
writeZipFile :: FilePath -> Map FilePath (FileContent m) -> IO ()
writeZipFile _fp _files = do
  pure ()

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

-- | writes a single file into a zip file
--   note that this doesn't write a fisih
singletonZipFile :: MonadResource m => FilePath -> FilePath -> FileInZipOptions -> ConduitT ByteString o m ()
singletonZipFile end zipPath filePath options = do
    bracketP (openFile zipPath WriteMode) hClose $ \handle ->
    fh <- liftIO $ appendLocalFileHeader h filePath options
    dd <- sinkData h $ fizCompression options
    liftIO $ writeDataDescriptorFields h dd offset
    pure dd
  where
    offset = fromIntegral $ zipCentralDirectoryOffset zip

-- TODO append files


writeFinish :: Handle -> CentralDirectory -> End -> IO ()
writeFinish h centralDir end = do
    writeCentralDirectory h centralDir
    writeEnd h
             (length $ cdFileHeaders centralDir)                      -- total number of entries in the central directory on this disk
             end


appendLocalFileHeader :: Handle -> FilePath -> FileInZipOptions -> IO FileHeader
appendLocalFileHeader handle zip filePath options = do
    writeLocalFileHeader handle fh
    return fh
  where
    offset = fromIntegral $ endCentralDirectoryOffset end
    fh     = mkFileHeader f options (fromIntegral offset)

mkFileHeader :: FilePath -> FileInZipOptions -> Word32 -> FileHeader
mkFileHeader filePath options relativeOffset =
    FileHeader { fhBitFlag                = fizBitflag options
               , fhCompressionMethod      = fizCompression options
               , fhLastModified           = fizModification options
               , fhCRC32                  = 0
               , fhCompressedSize         = 0
               , fhUncompressedSize       = 0
               , fhInternalFileAttributes = 0
               , fhExternalFileAttributes = 0
               , fhRelativeOffset         = relativeOffset
               , fhFileName               = filePath
               , fhExtraField             = empty
               , fhFileComment            = empty
               }

sinkData :: MonadResource m
         => Handle -> CompressionMethod -> ConduitT ByteString Void m DataDescriptor
sinkData h compression = do
    ((uncompressedSize, crc32), compressedSize) <-
        case compression of
          NoCompression -> CI.zipSinks sizeCrc32Sink sizeDataSink
          Deflate       -> CI.zipSinks sizeCrc32Sink compressSink
    return DataDescriptor
               { ddCRC32            = crc32
               , ddCompressedSize   = fromIntegral compressedSize
               , ddUncompressedSize = fromIntegral uncompressedSize
               }
  where
    compressSink :: MonadResource m => ConduitT ByteString Void m Int
    compressSink = compress 6 (WindowBits (-15)) =$ sizeDataSink

    sizeCrc32Sink :: MonadResource m => ConduitT ByteString Void m (Int, Word32)
    sizeCrc32Sink =  CI.zipSinks sizeSink crc32Sink

    sizeDataSink :: MonadResource m => ConduitT ByteString Void m Int
    sizeDataSink  = fst <$> CI.zipSinks sizeSink (CB.sinkHandle h)
