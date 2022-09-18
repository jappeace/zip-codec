{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions for writing entire zip files
module Zip.Codec.Write
  ( sinkFile
  -- * config helpers
  , FileInZipOptions(..)
  , fromFileHeader
  )
where

import Zip.Codec.Compress
import Zip.Codec.Time
import qualified Data.Map as Map
import Zip.Codec.DataDescriptor
import Data.Text(Text)
import Zip.Codec.End
import Zip.Codec.FileHeader
import Data.Void(Void)
import           Prelude hiding (readFile, zip)
import           Data.ByteString (ByteString)
import           Data.Time (UTCTime(..))
import           Data.Word
import           System.IO (Handle, IOMode(..), SeekMode(..), hSeek, openFile, hClose)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Conduit (ConduitT, (.|), bracketP, fuseUpstream)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import Zip.Codec.CentralDirectory
import Control.Monad.Primitive
import Control.Monad.Catch (MonadThrow)

-- | writes a single file into a zip file
--   note that this doesn't write a fisih
sinkFile :: (MonadResource m, PrimMonad m, MonadThrow m) => CentralDirectory -> End -> FilePath ->  FilePath -> FileInZipOptions -> ConduitT ByteString Void m (CentralDirectory, End)
sinkFile existingCentralDir end zipPath filePath options =
    bracketP (openFile zipPath ReadWriteMode) hClose $ \handle -> do
    fileHeaderOld <- liftIO $ appendLocalFileHeader handle end filePath options
    dd <- compressData (fizCompression options) `fuseUpstream` sinkDataHandle handle
    let newCentralDir = mkNewCentralDir fileHeader
        fileHeader = updateFileHeader dd fileHeaderOld
        newEnd = updateEnd dd fileHeader end

    liftIO $ do
      writeDataDescriptorFields handle dd offset
      writeFinish handle newCentralDir newEnd

    pure (newCentralDir, newEnd)
  where
    offset = fromIntegral $ endCentralDirectoryOffset end

    mkNewCentralDir header = CentralDirectory $
      Map.insert filePath header $ cdFileHeaders existingCentralDir

updateEnd :: DataDescriptor -> FileHeader -> End -> End
updateEnd dd fh end = end {
    endCentralDirectoryOffset = endCentralDirectoryOffset end
                            + (localFileHeaderLength fh + ddCompressedSize dd)
  , endCentralDirectorySize = endCentralDirectorySize end + (fileHeaderLength fh)
  }

updateFileHeader :: DataDescriptor -> FileHeader -> FileHeader
updateFileHeader dd fh = fh { fhDataDescriptor = dd
                            }

writeFinish :: Handle -> CentralDirectory -> End -> IO ()
writeFinish h centralDir end = do
    writeCentralDirectory h centralDir
    writeEnd h $ end{ endEntriesCount = fromIntegral (length $ cdFileHeaders centralDir) }


appendLocalFileHeader :: Handle -> End -> FilePath -> FileInZipOptions -> IO FileHeader
appendLocalFileHeader handle end filePath options = do
    hSeek handle AbsoluteSeek offset -- this  appears to override the previous central dir
    writeLocalFileHeader handle fh
    return fh
  where
    offset = fromIntegral $ endCentralDirectoryOffset end
    fh     = mkFileHeader filePath options (fromIntegral offset)

mkFileHeader :: FilePath -> FileInZipOptions -> Word32 -> FileHeader
mkFileHeader filePath options relativeOffset =
    FileHeader { fhBitFlag                = fizBitflag options
               , fhCompressionMethod      = fizCompression options
               , fhLastModified           = utcTimeToMSDOSDateTime $ fizModification options
               , fhDataDescriptor         = emptyDataDescriptor
               , fhInternalFileAttributes = 0
               , fhExternalFileAttributes = 0
               , fhRelativeOffset         = relativeOffset
               , fhFileName               = filePath
               , fhExtraField             = mempty
               , fhFileComment            = mempty
               }


data FileInZipOptions = MkFileInZipOptions {
    fizCompression  :: CompressionMethod
    -- | the modification time, not that this is clamped to 'MSDOSDateTime' (silently)
    --   see 'utcTimeToMSDOSDateTime' for details
  , fizModification :: UTCTime
  , fizBitflag      :: Word16
  , fizExtraField   :: ByteString
  , fizComment      :: Text
  } deriving (Show, Eq)

fromFileHeader :: FileHeader -> FileInZipOptions
fromFileHeader FileHeader{..} =
  MkFileInZipOptions
  { fizCompression  = fhCompressionMethod
  , fizModification = msDOSDateTimeToUTCTime fhLastModified
  , fizBitflag      = fhBitFlag
  , fizExtraField   = fhExtraField
  , fizComment      = fhFileComment
  }

sinkDataHandle :: forall m . (PrimMonad m, MonadResource m) => Handle -> ConduitT Compressed Void m ()
sinkDataHandle handle' =
  CC.map unCompressedChunck .|  CB.sinkHandle handle'
