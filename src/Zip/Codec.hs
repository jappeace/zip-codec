{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | High level interface for reading/writing zipfiles.
module Zip.Codec
  (
    -- * reading
    CodecErrors(..)
  , readZipFile
  , FileContent(..)
    -- * writing
  , writeZipFile
  , defOptions
  , fromFileHeader
  , appendBytestring
  )
where

import qualified Data.Map as Map
import Control.Monad.Trans.Class
import Data.Traversable
import Control.Monad.Trans.State.Lazy
import Data.Conduit(runConduitRes)
import Control.Exception
import Zip.Codec.End
import Zip.Codec.FileHeader
import           Prelude hiding (readFile, zip)
import           Data.ByteString (ByteString)
import           Data.Time (UTCTime(..))
import           System.IO (Handle, IOMode(..), SeekMode(..), hSeek, openFile, withFile)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Conduit (ConduitT, (.|), yield)
import Data.Map(Map)
import Zip.Codec.CentralDirectory
import Control.Monad.Trans.Except
import Data.Bifunctor
import Control.Monad.Primitive
import Zip.Codec.Read
import Zip.Codec.Write

data CodecErrors = FailedEndReading String
                 | FailedCentralDirectoryReading CenteralDirErrors
                 deriving (Show, Exception)

data FileContent m = MkFileContent
  { fcFileHeader :: FileInZipOptions -- ^ simplified representation of FileHeader
  , fcFileContents :: ConduitT () ByteString m ()
  }

-- | this opens up a file from the filesystem
--   it provides a map of internal file with a conduit to the data
readZipFile :: (MonadThrow m, PrimMonad m, MonadResource m) => FilePath -> IO (Either CodecErrors (Map FilePath (FileContent m)))
readZipFile zipPath =
    withFile zipPath ReadMode $ \handle' -> runExceptT $ do
      liftIO $ putStrLn "readnig end"
      end <- except . first FailedEndReading =<< liftIO (readEnd handle')
      liftIO $ putStrLn "reading central dir"
      central <- except . first FailedCentralDirectoryReading =<< liftIO (readCentralDirectory handle' end)
      liftIO $ putStrLn "returning conduit"
      pure $
        (\header -> ( MkFileContent
                  { fcFileHeader = fromFileHeader header
                  , fcFileContents = sourceFile zipPath header
                  })) <$> cdFileHeaders central


-- | appends a bytestring to the content conduit
appendBytestring :: Monad m => ByteString -> FileContent m -> FileContent m
appendBytestring bs opts = opts{ fcFileContents = fcFileContents opts <> yield bs }

-- shouldn't this return a map of sinks instead?
-- I'm not sure how finilzation works then
writeZipFile :: FilePath -> Map FilePath (FileContent (ResourceT IO)) -> IO (CentralDirectory, End)
writeZipFile zipPath filesMap = do

  nya <- flip execStateT (emptyCentralDirectory, emptyEnd) $
        forM files $ \(filePath, fileContents) -> do
           (centralDir, end) <- get
           res <- lift $ runConduitRes $
             fcFileContents fileContents .|
             sinkFile centralDir end zipPath filePath (fcFileHeader fileContents)
           put res
  pure nya

  where
    files :: [(FilePath, FileContent (ResourceT IO))]
    files = Map.toList filesMap

defOptions :: Monad m => FileContent m
defOptions = MkFileContent
    { fcFileHeader = MkFileInZipOptions
      { fizCompression  = Deflate
      , fizModification = UTCTime { utctDay = toEnum 0, utctDayTime = 0}
      , fizBitflag      = 2  -- max compression for deflate compression method
      , fizExtraField   = mempty
      , fizComment      = mempty
      }
    , fcFileContents     = yield mempty
    }
