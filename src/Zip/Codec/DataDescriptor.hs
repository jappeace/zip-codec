{-# LANGUAGE RecordWildCards #-}

-- | This is used to indicate the shape of a file,
--   it includes a checksum, compressed and uncompressed sizes.
module Zip.Codec.DataDescriptor
  ( DataDescriptor(..)
  , writeDataDescriptor
  , putDataDescriptor
  , getDataDescriptor
  , emptyDataDescriptor
  )
where

import qualified Data.ByteString as B
import           Data.Serialize
import           Data.Word
import           System.IO

-- | Data descriptor
--
-- crc-32                          4 bytes
-- compressed size                 4 bytes -- 8 bytes for 64 bit
-- uncompressed size               4 bytes -- 8 bytes for 64 bit
--
-- This appears to be used to track the end offset of the zipfile
data DataDescriptor = DataDescriptor
    { ddCRC32            :: Word32
    , ddCompressedSize   :: Word32
    , ddUncompressedSize :: Word32
    } deriving (Show, Eq)

emptyDataDescriptor :: DataDescriptor
emptyDataDescriptor = DataDescriptor
    { ddCRC32            = 0
    , ddCompressedSize   = 0
    , ddUncompressedSize = 0
    }

writeDataDescriptor :: Handle -> DataDescriptor -> IO ()
writeDataDescriptor h dd =
    B.hPut h $ runPut $ putDataDescriptor dd

getDataDescriptor :: Get DataDescriptor
getDataDescriptor = do
    ddCRC32            <- getWord32le
    ddCompressedSize   <- getWord32le
    ddUncompressedSize <- getWord32le
    pure $ DataDescriptor {..}

putDataDescriptor :: DataDescriptor -> Put
putDataDescriptor dd = do
--    putWord32le 0x08074b50
    putWord32le $ ddCRC32 dd
    putWord32le $ ddCompressedSize dd
    putWord32le $ ddUncompressedSize dd
