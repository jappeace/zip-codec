{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions for reading entire zipfiles
module Zip.Codec.Read
  ( sourceFile
  , SourceFileError(..)
  )
where

import           Zip.Codec.DataDescriptor
import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.Primitive
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.ByteString              (ByteString)
import           Data.Conduit                 (ConduitT, (.|))
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import           Data.Conduit.Zlib            (WindowBits (..), decompress)
import           Prelude                      hiding (readFile, zip)
import           System.IO
import           Zip.Codec.FileHeader


data SourceFileError = OffsetError String
  deriving stock Show
  deriving anyclass Exception

sourceFile :: (MonadThrow m, PrimMonad m, MonadResource m) => FilePath -> FileHeader -> ConduitT () ByteString m ()
sourceFile zipPath fileHeader =
    source .| CB.isolate (fromIntegral $ ddCompressedSize $ fhDataDescriptor fileHeader)
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
