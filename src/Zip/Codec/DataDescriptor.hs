module Zip.Codec.DataDescriptor
  ( DataDescriptor(..)
  , writeDataDescriptorFields
  , writeDataDescriptor
  , putDataDescriptor
  )
where

import           System.IO (Handle, SeekMode(..), hFileSize, hSeek, hTell)
import Data.Word
import           Data.Serialize (Get, Put, getByteString, getWord16le, getWord32le, putByteString, putWord16le, putWord32le, runPut, skip)
import qualified Data.ByteString as B

-- | Data descriptor
--
-- crc-32                          4 bytes
-- compressed size                 4 bytes
-- uncompressed size               4 bytes
--
-- This appears to be used to track the end offset of the zipfile
data DataDescriptor = DataDescriptor
    { ddCRC32            :: Word32
    , ddCompressedSize   :: Word32
    , ddUncompressedSize :: Word32
    } deriving (Show)


writeDataDescriptor :: Handle -> DataDescriptor -> IO ()
writeDataDescriptor h dd =
    B.hPut h $ runPut $ putDataDescriptor dd


putDataDescriptor :: DataDescriptor -> Put
putDataDescriptor dd = do
--    putWord32le 0x08074b50
    putWord32le $ ddCRC32 dd
    putWord32le $ ddCompressedSize dd
    putWord32le $ ddUncompressedSize dd

-- | Writes data descriptor fields (crc-32, compressed size and
-- uncompressed size) in the middle of the local file header.
writeDataDescriptorFields :: Handle -> DataDescriptor -> Integer -> IO ()
writeDataDescriptorFields h dd offset = do
    old <- hTell h
    hSeek h AbsoluteSeek $ offset + 4 + 2 + 2 + 2 + 2 + 2
    writeDataDescriptor h dd
    hSeek h AbsoluteSeek old
