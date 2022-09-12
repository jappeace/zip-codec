-- | Every file in a zipfile has a header giving some meta data
--   on what is in the file. such as compression method and the name.
module Zip.Codec.FileHeader
  ( getFileHeader
  , FileHeader(..)
  , calculateFileDataOffset
  , CompressionMethod(..)
  , GetFileHeaderError(..)
  , writeLocalFileHeader
  , putLocalFileHeader
  , putFileHeader
  , localFileHeaderLength
  , fileHeaderLength
  )
where

import Zip.Codec.DataDescriptor
import Data.Text(Text)
import           System.IO (Handle, SeekMode(..), hSeek)
import Data.Serialize.Get
import           Data.Serialize
import Zip.Codec.Time
import           Data.Word (Word16, Word32)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text.Encoding(decodeUtf8', encodeUtf8)
import           Control.Monad (unless)
import Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Text as T
import Data.Bifunctor

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
    , fhLastModified           :: MSDOSDateTime
    , fhDataDescriptor         :: DataDescriptor
    , fhInternalFileAttributes :: Word16
    , fhExternalFileAttributes :: Word32
    , fhRelativeOffset         :: Word32
    -- | note that the central directory map overwrites this atribute,
    --   but we need it for reading
    , fhFileName               :: FilePath
    , fhExtraField             :: ByteString
    , fhFileComment            :: Text
    } deriving (Show, Eq)


data CompressionMethod = NoCompression
                       | Deflate
                         deriving (Show, Eq, Bounded, Enum)

data GetFileHeaderError = DecodeFileNameFailed { original :: ByteString, exception :: UnicodeException}
                        | DecodeCommentFailed { original :: ByteString, exception :: UnicodeException}
                        deriving (Show, Eq)

getFileHeader :: Get (Either GetFileHeaderError FileHeader)
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
    dataDescriptor         <- getDataDescriptor
    fileNameLength         <- fromIntegral <$> getWord16le
    extraFieldLength       <- fromIntegral <$> getWord16le
    fileCommentLength      <- fromIntegral <$> getWord16le
    skip 2
    internalFileAttributes <- getWord16le
    externalFileAttributes <- getWord32le
    relativeOffset         <- getWord32le
    fileName               <- getByteString fileNameLength
    extraField             <- getByteString extraFieldLength
    fileComment            <- getByteString fileCommentLength
    return $ do
      fname <- first (DecodeFileNameFailed fileName) $ decodeUtf8' fileName
      comment <- first (DecodeCommentFailed  fileComment) $ decodeUtf8' fileComment
      pure $ FileHeader
               { fhBitFlag                = bitFlag
               , fhCompressionMethod      = compessionMethod
               , fhLastModified           = MSDOSDateTime { msDOSDate = lastModFileDate, msDOSTime = lastModFileTime}
               , fhDataDescriptor         = dataDescriptor
               , fhInternalFileAttributes = internalFileAttributes
               , fhExternalFileAttributes = externalFileAttributes
               , fhRelativeOffset         = relativeOffset
               , fhFileName               = T.unpack fname
               , fhExtraField             = extraField
               , fhFileComment            = comment
               }

calculateFileDataOffset :: Handle -> FileHeader -> IO (Either String Integer)
calculateFileDataOffset h fh = do
    lfhLength <- readLocalFileHeaderLength h fh
    return $ (fromIntegral (fhRelativeOffset fh) +) <$> lfhLength

readLocalFileHeaderLength :: Handle -> FileHeader -> IO (Either String Integer)
readLocalFileHeaderLength h header =
    runGet getLocalFileHeaderLength <$> hGetLocalFileHeader h header


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

localFileHeaderConstantLength :: Int
localFileHeaderConstantLength = 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2

signature :: Word32 -> Get ()
signature sig = do
    s <- lookAhead getWord32le
    if s == sig
      then skip 4
      else fail "Wrong signature."

hGetLocalFileHeader :: Handle -> FileHeader -> IO ByteString
hGetLocalFileHeader h fh = do
    hSeek h AbsoluteSeek offset
    B.hGet h localFileHeaderConstantLength
  where
    offset = fromIntegral $ fhRelativeOffset fh

writeLocalFileHeader :: Handle -> FileHeader -> IO ()
writeLocalFileHeader h fh =
    B.hPut h . runPut $ putLocalFileHeader fh


putLocalFileHeader :: FileHeader -> Put
putLocalFileHeader fh = do
    putWord32le 0x04034b50
    putWord16le 20  -- version needed to extract (>= 2.0)
    putFileHeaderMeta fh
    putFileHeaderNameExtra fh

putFileHeaderMeta :: FileHeader -> Put
putFileHeaderMeta fh = do
    putWord16le $ fhBitFlag fh
    putWord16le compressionMethod
    putWord16le $ msDOSTime modTime
    putWord16le $ msDOSDate modTime
    putDataDescriptor $ fhDataDescriptor fh
    putWord16le $ fromIntegral $ B.length $ encodeUtf8 $ T.pack $ fhFileName fh
    putWord16le $ fromIntegral $ B.length $ fhExtraField fh
  where
    modTime = fhLastModified fh
    compressionMethod = case fhCompressionMethod fh of
                          NoCompression -> 0
                          Deflate       -> 8

putFileHeaderNameExtra :: FileHeader -> Put
putFileHeaderNameExtra fh = do
    putByteString $ encodeUtf8 $ T.pack $ fhFileName fh
    putByteString $ fhExtraField fh

putFileHeader :: FileHeader -> Put
putFileHeader fh = do
    putWord32le 0x02014b50
    putWord16le 0   -- version made by
    putWord16le 20  -- version needed to extract (>= 2.0)
    putFileHeaderMeta fh
    putWord16le $ fromIntegral $ B.length $ encodeUtf8 $ fhFileComment fh
    putWord16le 0  -- disk number start
    putWord16le $ fhInternalFileAttributes fh
    putWord32le $ fhExternalFileAttributes fh
    putWord32le $ fhRelativeOffset fh
    putFileHeaderNameExtra fh
    putByteString $ encodeUtf8 $ fhFileComment fh

localFileHeaderLength :: FileHeader -> Word32
localFileHeaderLength fh =
  fromIntegral $ 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2
               + length (fhFileName fh) + B.length (fhExtraField fh)

fileHeaderLength :: FileHeader -> Word32
fileHeaderLength fh =
  fromIntegral $ 4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4
               + length (fhFileName fh) + B.length (fhExtraField fh)
               + B.length (encodeUtf8 $ fhFileComment fh)
