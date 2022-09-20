{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -w #-}

-- | Functions for writing entire zip files
module Zip.Codec.Write
  ( sinkFile
  -- * config helpers
  , FileInZipOptions(..)
  , fromFileHeader
  , writeFinish
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
import           System.IO (Handle)
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Conduit (ConduitT, (.|), fuseUpstream, yield)
import Data.Serialize.Put
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import Zip.Codec.CentralDirectory
import Control.Monad.Primitive
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class(liftIO)

-- | writes a single file into a zip file
--   note that this doesn't write a fisih
sinkFile :: (MonadResource m, PrimMonad m, MonadThrow m) => CentralDirectory -> End -> Handle ->  FilePath -> FileInZipOptions -> ConduitT ByteString Void m (CentralDirectory, End)
sinkFile existingCentralDir end handle filePath options = do
    dd <- noCentralDirSink handle end filePath options

    let newCentralDir = mkNewCentralDir fileHeader
        fileHeader = updateFileHeader dd fileHeaderOld
        newEnd = updateEnd dd fileHeader end

    pure (newCentralDir, newEnd)
  where
    fileHeaderOld = mkFileHeader filePath options $ endCentralDirectoryOffset end

    mkNewCentralDir header = CentralDirectory $
      Map.insert filePath header $ cdFileHeaders existingCentralDir


noCentralDirSink :: (MonadResource m, PrimMonad m, MonadThrow m) => Handle ->  End -> FilePath -> FileInZipOptions -> ConduitT ByteString Void m DataDescriptor
noCentralDirSink handle end filePath options = do
    fileInZipConduit filePath options (endCentralDirectoryOffset end)
      `fuseUpstream` sinkDataHandle handle

newtype FileInZip = MkFileInZip { unFileInZip :: ByteString }

-- | Use of this conduit requires setting bit 3 of the general purpose
--   bitflag
fileInZipConduit :: (MonadResource m, PrimMonad m, MonadThrow m) =>  FilePath -> FileInZipOptions -> Word32 -> ConduitT ByteString FileInZip m DataDescriptor
fileInZipConduit filePath options offset = do
    yield $ MkFileInZip $ runPut $ putLocalFileHeader fh
    dd <- compressData (fizCompression options) `fuseUpstream` CC.map (MkFileInZip . unCompressedChunck)
    yield $ MkFileInZip $ runPut $ do
      putWord32le 0x08074b50
      putDataDescriptor dd
    pure dd
    where
    fh     = mkFileHeader filePath options offset

updateEnd :: DataDescriptor -> FileHeader -> End -> End
updateEnd dd fh end = end {
    endCentralDirectoryOffset = endCentralDirectoryOffset end
                            + (localFileHeaderLength fh + ddCompressedSize dd) + 16
  , endCentralDirectorySize = endCentralDirectorySize end + (fileHeaderLength fh)
  }

updateFileHeader :: DataDescriptor -> FileHeader -> FileHeader
updateFileHeader dd fh = fh { fhDataDescriptor = dd
                            }

writeFinish :: Handle -> CentralDirectory -> End -> IO ()
writeFinish h centralDir end = do
    writeCentralDirectory h centralDir
    writeEnd h $ end{ endEntriesCount = fromIntegral (length $ cdFileHeaders centralDir) }


mkFileHeader :: FilePath -> FileInZipOptions -> Word32 -> FileHeader
mkFileHeader filePath options relativeOffset =
    FileHeader { fhBitFlag                = toggleDeferDataDescriptor $ toggleMaxCOmpressionDeflate unsertBitflag
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
  , fizExtraField   :: ByteString
  , fizComment      :: Text
  } deriving (Show, Eq)

fromFileHeader :: FileHeader -> FileInZipOptions
fromFileHeader FileHeader{..} =
  MkFileInZipOptions
  { fizCompression  = fhCompressionMethod
  , fizModification = msDOSDateTimeToUTCTime fhLastModified
  , fizExtraField   = fhExtraField
  , fizComment      = fhFileComment
  }

sinkDataHandle :: forall m . (PrimMonad m, MonadResource m) => Handle -> ConduitT FileInZip Void m ()
sinkDataHandle handle' =
  CC.map unFileInZip .|  CB.sinkHandle handle'
