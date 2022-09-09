module Zip.Codec.FileHeader
  ( getFileHeader
  , FileHeader(..)
  , calculateFileDataOffset
  )
where

import Data.Time
import           System.IO (Handle, SeekMode(..), hFileSize, hSeek, hTell)
import Data.Binary.Get
import Zip.Codec.Time

-- | File header:
--
-- central file header signature   4 bytes  (0x02014b50)
-- version made by                 2 bytes
-- version needed to extract       2 bytes
-- general purpose bit flag        2 bytes
-- compression method              2 bytes
-- last mod file time              2 bytes
-- last mod file date              2 bytes
-- crc-32                          4 bytes
-- compressed size                 4 bytes
-- uncompressed size               4 bytes
-- file name length                2 bytes
-- extra field length              2 bytes
-- file comment length             2 bytes
-- disk number start               2 bytes
-- internal file attributes        2 bytes
-- external file attributes        4 bytes
-- relative offset of local header 4 bytes
--
-- file name (variable size)
-- extra field (variable size)
-- file comment (variable size)
data FileHeader = FileHeader
    { fhBitFlag                :: Word16
    , fhCompressionMethod      :: CompressionMethod
    , fhLastModified           :: UTCTime
    , fhCRC32                  :: Word32
    , fhCompressedSize         :: Word32
    , fhUncompressedSize       :: Word32
    , fhInternalFileAttributes :: Word16
    , fhExternalFileAttributes :: Word32
    , fhRelativeOffset         :: Word32
    , fhFileName               :: FilePath
    , fhExtraField             :: ByteString
    , fhFileComment            :: ByteString
    } deriving (Show)


data CompressionMethod = NoCompression
                       | Deflate
                         deriving (Show)


getFileHeader :: Get FileHeader
getFileHeader = do
    signature 0x02014b50
    skip 2
    versionNeededToExtract <- getWord16le
    unless (versionNeededToExtract <= 20) $
        fail "This archive requires zip >= 2.0 to extract."
    bitFlag                <- getWord16le
    rawCompressionMethod   <- getWord16le
    compessionMethod       <- case rawCompressionMethod of
                                0 -> return NoCompression
                                8 -> return Deflate
                                _ -> fail $ "Unknown compression method "
                                          ++ show rawCompressionMethod
    lastModFileTime        <- getWord16le
    lastModFileDate        <- getWord16le
    crc32                  <- getWord32le
    compressedSize         <- fromIntegral <$> getWord32le
    uncompressedSize       <- getWord32le
    fileNameLength         <- fromIntegral <$> getWord16le
    extraFieldLength       <- fromIntegral <$> getWord16le
    fileCommentLength      <- fromIntegral <$> getWord16le
    skip 2
    internalFileAttributes <- getWord16le
    externalFileAttributes <- getWord32le
    relativeOffset         <- fromIntegral <$> getWord32le
    fileName               <- getByteString fileNameLength
    extraField             <- getByteString extraFieldLength
    fileComment            <- getByteString fileCommentLength
    return FileHeader
               { fhBitFlag                = bitFlag
               , fhCompressionMethod      = compessionMethod
               , fhLastModified           = toUTC lastModFileDate lastModFileTime
               , fhCRC32                  = crc32
               , fhCompressedSize         = compressedSize
               , fhUncompressedSize       = uncompressedSize
               , fhInternalFileAttributes = internalFileAttributes
               , fhExternalFileAttributes = externalFileAttributes
               , fhRelativeOffset         = relativeOffset
               , fhFileName               = toString fileName
               , fhExtraField             = extraField
               , fhFileComment            = fileComment
               }
  where
    toUTC date time =
        msDOSDateTimeToUTCTime MSDOSDateTime { msDOSDate = date
                                             , msDOSTime = time
                                             }

calculateFileDataOffset :: Handle -> FileHeader -> IO Integer
calculateFileDataOffset h fh = do
    lfhLength <- readLocalFileHeaderLength h fh
    return $ fromIntegral (fhRelativeOffset fh) + lfhLength

readLocalFileHeaderLength :: Handle -> FileHeader -> IO Integer
readLocalFileHeaderLength h header =
    runGet' getLocalFileHeaderLength <$> hGetLocalFileHeader h header


-- Gets length of the local file header, i.e. sum of lengths of its
-- constant and variable parts.
getLocalFileHeaderLength :: Get Integer
getLocalFileHeaderLength = do
    signature 0x04034b50
    skip $ 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4
    fileNameLength    <- fromIntegral <$> getWord16le
    extraFieldLength  <- fromIntegral <$> getWord16le

    return $ fromIntegral localFileHeaderConstantLength
           + fileNameLength
           + extraFieldLength