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
  , readFileContent
  , readFileContentMap
    -- ** expert usage
  , readEndAndCentralDir
    -- * writing
  , writeZipFile
  , writeZipFileAsync
  , defOptions
  , fromFileHeader
  , appendBytestring
  )
where

import Data.Foldable(foldl')
import Zip.Codec.OSFile (concatManyAsync)
import System.Directory(renameFile)
import           System.IO (Handle, IOMode(..), openFile, hClose, openTempFile)
import Zip.Codec.Time
import qualified Data.Map as Map
import Control.Monad.Trans.Class
import Data.Traversable
import Control.Monad.Trans.State.Lazy
import Data.Conduit(runConduitRes, ConduitT, (.|), yield)
import qualified Data.Conduit.Combinators as CC
import Control.Exception hiding (handle)
import Zip.Codec.End
import Zip.Codec.FileHeader
import           Prelude hiding (readFile, zip)
import           Data.ByteString (ByteString)
import           Data.Time (UTCTime(..))
import           System.IO (withFile)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource
import Data.Map(Map)
import Zip.Codec.CentralDirectory
import Control.Monad.Trans.Except
import Data.Bifunctor
import Control.Monad.Primitive
import Zip.Codec.Read
import Zip.Codec.Write
import UnliftIO.Temporary (withSystemTempDirectory)
import Control.Concurrent.Async(forConcurrently)

data CodecErrors = FailedEndReading String
                 | FailedCentralDirectoryReading CenteralDirErrors
                 deriving (Show, Exception, Eq)

data FileContent m = MkFileContent
  { fcFileHeader :: FileInZipOptions -- ^ simplified representation of FileHeader
  , fcFileContents :: ConduitT () ByteString m ()
  }

-- | this opens up a file from the filesystem
--   it provides a map of internal file with a conduit to the data
readZipFile :: (MonadThrow m, PrimMonad m, MonadResource m) => FilePath -> IO (Either CodecErrors (Map FilePath (FileContent m)))
readZipFile zipPath =
  do
    eCentral <- readEndAndCentralDir zipPath
    pure $ do
        (_end, central) <- eCentral
        pure $ (\header -> ( MkFileContent
                  { fcFileHeader = fromFileHeader header
                  , fcFileContents = sourceFile zipPath header
                  })) <$> cdFileHeaders central

readEndAndCentralDir :: FilePath -> IO (Either CodecErrors (End, CentralDirectory))
readEndAndCentralDir zipPath =
    withFile zipPath ReadMode $ \handle' -> runExceptT $ do
      end <- except . first FailedEndReading =<< liftIO (readEnd handle')
      central <- except . first FailedCentralDirectoryReading =<< liftIO (readCentralDirectory handle' end)
      pure (end, central)

-- | appends a bytestring to the content conduit
appendBytestring :: Monad m => ByteString -> FileContent m -> FileContent m
appendBytestring bs opts = opts{ fcFileContents = fcFileContents opts <> yield bs }

-- | read a file from the file system into a filecontent
readFileContent :: FilePath -> FileContent (ResourceT IO)
readFileContent filepath = x
                                { fcFileContents = CC.sourceFile filepath }
                                where
                                  x :: FileContent (ResourceT IO)
                                  x = defOptions

-- | does 'readFileContent' and puts it into a singleton map under the same filename
readFileContentMap :: FilePath -> Map FilePath (FileContent (ResourceT IO))
readFileContentMap x = Map.singleton x $ readFileContent x

-- shouldn't this return a map of sinks instead?
-- I'm not sure how finilzation works then
writeZipFile ::
  -- | Path to the zipfile to be written
  FilePath ->
  -- | A map with as key the filename of the zipfile and value the content desscription of a file.
  Map FilePath (FileContent (ResourceT IO)) ->
  IO ()
writeZipFile zipPath filesMap = do
  bracket (openFile zipPath ReadWriteMode) hClose $ \handle -> do
    (newCentralDir, newEnd) <- flip execStateT (emptyCentralDirectory, emptyEnd) $
          forM files $ \curFile -> do
            state' <- get
            res <- lift $ writeFileContent handle state' curFile
            put (sfrCentralDirectory res, sfrEnd res)

    liftIO $ writeFinish handle newCentralDir newEnd
  where
    files :: [(FilePath, FileContent (ResourceT IO))]
    files = Map.toList filesMap

data ConcFileRes = MkConcFileRes
  { cfrInZipPath :: FilePath
  , cfrDiskPath  :: FilePath
  , cfrSinkFileResult :: SinkFileResult
  }

writeZipFileAsync ::
  -- | Path to the zipfile to be written
  FilePath ->
  -- | A map with as key the filename of the zipfile and value the content desscription of a file.
  Map FilePath (FileContent (ResourceT IO)) ->
  IO ()
writeZipFileAsync zipPath filesMap = withSystemTempDirectory "writeZipFileAsync" $ \tempDir -> do
  outputs <- forConcurrently files $ \contents -> do
    let cfrInZipPath = fst contents
    bracket (openTempFile tempDir cfrInZipPath) (hClose . snd) $ \(cfrDiskPath, handle) -> do
      cfrSinkFileResult <- writeFileContent handle (emptyCentralDirectory, emptyEnd) contents
      pure $ MkConcFileRes{..}

  finalFile <- concatManyAsync $ cfrDiskPath <$> outputs

  let endDir :: (End, CentralDirectory)
        = foldl' updateDir (emptyEnd, emptyCentralDirectory) outputs

  renameFile finalFile zipPath
  withFile zipPath AppendMode $ \handle -> writeFinish handle (snd endDir) (fst endDir)


  where
    files :: [(FilePath, FileContent (ResourceT IO))]
    files = Map.toList filesMap


updateDir :: (End, CentralDirectory) -> ConcFileRes -> (End, CentralDirectory)
updateDir (prevEnd, prevDir) current =
  (prevEnd <> curEnd, insertFile path curHeader prevDir)
  where
    curEnd = sfrEnd sinkFile
    curHeader =
      (sfrFileHeader sinkFile) { fhRelativeOffset = endCentralDirectoryOffset prevEnd }
    path = cfrInZipPath current
    sinkFile = cfrSinkFileResult current

-- | Write a single file content to a zipfile.
writeFileContent ::
  -- | the path to the zipfile to be written
  Handle ->
  -- | previous central directory and end
  (CentralDirectory, End) ->
  -- | the file to be written within a zipfile
  (FilePath, FileContent (ResourceT IO)) ->
  -- | new central directory and end
  IO SinkFileResult
writeFileContent handle (centralDir, end) (filePath, fileContents) =
  runConduitRes $
             fcFileContents fileContents .|
             sinkFile centralDir end handle filePath (fcFileHeader fileContents)

-- | empty file content
defOptions :: Monad m => FileContent m
defOptions = MkFileContent
    { fcFileHeader = MkFileInZipOptions
      { fizCompression  = Deflate
      , fizModification = msDOSDateTimeToUTCTime $ utcTimeToMSDOSDateTime (UTCTime { utctDay = toEnum 0, utctDayTime = 0})
      , fizExtraField   = mempty
      , fizComment      = mempty
      }
    , fcFileContents     = yield mempty
    }

