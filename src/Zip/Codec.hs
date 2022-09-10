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
  , FileWriteOptions(..)
  , defOptions
  , fromFileHeader
  , appendBytestring
  )
where

import Data.Text(Text)
import Control.Exception
import Zip.Codec.End
import Zip.Codec.FileHeader
import           Prelude hiding (readFile, zip)
import           Data.ByteString (ByteString)
import           Data.Time (UTCTime(..))
import           Data.Word
import           System.IO (IOMode(..), SeekMode(..), hSeek, openFile, withFile)
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
  { fcFileHeader :: FileHeader
  , fcData :: ConduitT () ByteString m ()
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
                  { fcFileHeader = header
                  , fcData = sourceFile zipPath header
                  })) <$> cdFileHeaders central

data FileInZipOptions = MkFileInZipOptions {
    fizCompression  :: CompressionMethod
  , fizModification :: UTCTime
  , fizBitflag      :: Word16
  , fizExtraField   :: ByteString
  , fizComment      :: Text
  } deriving (Show, Eq)

data FileWriteOptions m = MkFileWriteOptions {
    foFileOptions      ::  FileInZipOptions
  , foFileContents     :: ConduitT () ByteString m ()
  }



defOptions :: Monad m => FileWriteOptions m
defOptions = MkFileWriteOptions
    { foFileOptions = MkFileInZipOptions
      { fizCompression  = Deflate
      , fizModification = UTCTime { utctDay = toEnum 0, utctDayTime = 0}
      , fizBitflag      = 0
      , fizExtraField   = mempty
      , fizComment      = mempty
      }
    , foFileContents     = yield mempty
    }

-- | appends a bytestring to the content conduit
appendBytestring :: Monad m => ByteString -> FileWriteOptions m -> FileWriteOptions m
appendBytestring bs opts = opts{ foFileContents = foFileContents opts <> yield bs }


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
writeZipFile :: FilePath -> Map FilePath (FileWriteOptions m) -> IO ()
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
