module Zip.Codec
  ( CodecErrors(..)
  , withArchive
  )
where


import           System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime)
import qualified Data.Bytesring.Strict as S
import Zip.Codec.End

data CodecErrors = FailedEndReading (GetResult String)
                 | FailedCentralDirectoryReading (GetResult String)

-- | this opens up a file from the filesystem
withArchive :: MonadResource m => FilePath -> IO (Either CodecErrors (Map FilePath (ConduitT () S.ByteString m ())))
withArchive zipPath =
    withFile zipPath ReadMode $ \handle -> runExceptT do
      end <- except . FailedEndReading <$> readEnd handle
      central <- except . FailedCentralDirectoryReading <$> readCentralDirectory h e

      pure $
        sourceFile zipPath <$> cdFileHeaders central

sourceFile :: MonadResource m => FilePath -> FileHeader -> ConduitT () ByteString m ()
sourceFile zipPath fileHeader =
    source $= CB.isolate (fromIntegral $ fhCompressedSize fileHeader)
           $= decomp
  where
    source = CB.sourceIOHandle $ do
        h      <- openFile (zipFilePath zip) ReadMode
        offset <- calculateFileDataOffset h fileHeader
        hSeek h AbsoluteSeek offset
        return h

    decomp =
        case fhCompressionMethod fileHeader of
          NoCompression -> CL.map id
          Deflate       -> decompress $ WindowBits (-15)
