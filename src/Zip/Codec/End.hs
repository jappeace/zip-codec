-- | The end of a zipfile has the directory layout.
--   this module helps the decoder figure out where that is quickly
--   by tracking the offset.
--
--   when decoding the first thing we have to do is find the End
--   (marked by a signature).
--   when encoding, for every file we have to keep on moving the offset
--   back
module Zip.Codec.End
  ( End(..)
  , readEnd
  , getEnd
  )
where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize.Get
import           System.IO (Handle, SeekMode(..), hFileSize, hSeek, hTell)


-- | End of central directory record:
--
-- end of central dir signature    4 bytes  (0x06054b50)
-- number of this disk             2 bytes
-- number of the disk with the
-- start of the central directory  2 bytes
-- total number of entries in the
-- central directory on this disk  2 bytes
-- total number of entries in
-- the central directory           2 bytes
-- size of the central directory   4 bytes
-- offset of start of central
-- directory with respect to
-- the starting disk number        4 bytes
-- .ZIP file comment length        2 bytes
-- .ZIP file comment       (variable size)
data End = End
    { endCentralDirectorySize   :: Int
    , endCentralDirectoryOffset :: Int
    , endZipComment             :: ByteString
    } deriving (Show)


readEnd :: Handle -> IO (Either String End)
readEnd h =
    runGet getEnd <$> hGetEnd h

getEnd :: Get End
getEnd = do
   skip $ 2 + 2 + 2 + 2
   size          <- fromIntegral <$> getWord32le
   offset        <- fromIntegral <$> getWord32le
   commentLength <- fromIntegral <$> getWord16le
   comment       <- getByteString commentLength
   return End { endCentralDirectorySize   = size
              , endCentralDirectoryOffset = offset
              , endZipComment             = comment
              }


-- TODO: find a better way to find the end of central dir signature
hGetEnd :: Handle -> IO ByteString
hGetEnd h = do
    hSeek h SeekFromEnd (-4)
    loop
  where
    loop = do
        s <- B.hGet h 4

        if s == B.pack (reverse [0x06, 0x05, 0x4b, 0x50])
          then get
          else next

    get = do
        size   <- hFileSize h
        offset <- hTell h
        B.hGet h $ fromIntegral (size - offset)

    next = do
        hSeek h RelativeSeek (-5)
        loop
