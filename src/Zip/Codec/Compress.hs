{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions for compressing data
module Zip.Codec.Compress
  ( compressData
  , Compressed
  , unCompressedChunck
  )
where

import Zip.Codec.DataDescriptor
import Zip.Codec.FileHeader
import qualified Data.ByteString as B
import           Data.Digest.CRC32 (crc32Update)
import           Prelude hiding (readFile, zip)
import           Data.ByteString (ByteString)
import           Data.Word
import           Data.Conduit (ConduitT, (.|), ZipConduit(..), fuseUpstream)
import qualified Data.Conduit.Combinators as CC
import           Data.Conduit.Zlib (WindowBits(..), compress)
import Control.Monad.Primitive
import Control.Monad.Catch (MonadThrow)
import qualified Data.Conduit.List as CL

newtype Compressed =
  MkCompressed { unCompressedChunck ::  ByteString}

-- | compresses data according to compression method
--   the datadescriptor is used to figure out the end and central directory
compressData :: forall m . (PrimMonad m, MonadThrow m)
         => CompressionMethod -> ConduitT ByteString Compressed m DataDescriptor
compressData compression = do
    ((uncompressedSize, crc32), compressedSize) <- getZipConduit compressionZip
    return DataDescriptor
               { ddCRC32            = crc32
               , ddCompressedSize   = fromIntegral compressedSize
               , ddUncompressedSize = fromIntegral uncompressedSize
               }

  where
    -- these zip conduits aren't to be confused with zip files.
    -- all they do is distribut input across various downstream conduit.
    -- (like a izplist, rather then cartesian)
    compressionZip :: ZipConduit ByteString Compressed m ((Int, Word32), Int)
    compressionZip =
              case compression of
                NoCompression -> (,) <$> sizeCrc32Fold <*> (ZipConduit $ tagSize `fuseUpstream` CC.map MkCompressed)
                Deflate       -> (,) <$> sizeCrc32Fold <*> ZipConduit compressSizeFold


sizeCrc32Fold :: (PrimMonad m) => ZipConduit ByteString o m (Int, Word32)
sizeCrc32Fold =  (,) <$> sizeFold <*> crc32Fold

sizeFold :: Monad m => ZipConduit ByteString o m Int
sizeFold = ZipConduit $
  tagSize `fuseUpstream` CC.sinkNull


compressSizeFold :: (PrimMonad m, MonadThrow m) => ConduitT ByteString Compressed m Int
compressSizeFold =
  (applyCompression .| tagSize `fuseUpstream` CC.map MkCompressed)  -- this seems dumb
    where applyCompression = compress 6 (WindowBits (-15))

crc32Fold :: PrimMonad m => ZipConduit ByteString o m Word32
crc32Fold = ZipConduit $
    CC.foldl crc32Update 0

-- we need to allow the input to bypass the counting so we don't have
-- to run compression twice (which is dumb)
tagSize :: Monad m => ConduitT ByteString ByteString m Int
tagSize =
    CL.mapAccum (\input acc -> (B.length input + acc, input)) 0
