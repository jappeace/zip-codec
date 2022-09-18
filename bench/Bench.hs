{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Functor.WithIndex
import Data.Foldable(fold)
import qualified Data.Map as Map
import           Control.Monad (forM_, void)
import Data.Traversable(forM)
import qualified Data.ByteString.Lazy as B (hPut, pack, readFile)
import           Data.Word (Word8)
import           System.FilePath ((</>), (<.>))
import           System.IO (IOMode(..), hPutStr, withFile)
import           System.Directory (getCurrentDirectory, setCurrentDirectory)

import Data.Time
import           Criterion.Main (Benchmark, bench, bgroup, defaultMain, nfIO)
import           System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import           System.Random (getStdGen, randoms)

import Data.Traversable.WithIndex
import Control.Exception
import qualified Data.Conduit.Combinators as CC
import Data.Conduit ((.|), runConduitRes, yield)
import qualified Codec.Archive.LibZip as L
import qualified "zip-archive" Codec.Archive.Zip as A
import qualified "zip" Codec.Archive.Zip as Zip
import qualified "zip-stream" Codec.Archive.Zip.Conduit.Zip as Stream
import qualified "zip-stream" Codec.Archive.Zip.Conduit.UnZip as Stream

import qualified Zip.Codec as This
import qualified Data.ByteUnits as Byte
import qualified Control.Concurrent.Async as Async
import qualified Data.Text as Text


main :: IO ()
main = do
    let singleSizes = [
            1024*1024
          , 10*1024*1024
          ]    -- ^ sizes of files for benchmarking
    let multipleSizes = [
          1024*1024
          , 10*1024*1024
          , 7*1024*1024
          , 3*1024*1024
          , 4*1024*1024
          , 5*1024*1024
          , 6*1024*1024
          ]

    withSystemTempDirectory "zip-conduit" $ \dir -> do
        prepareFiles dir singleSizes
        prepareFiles dir multipleSizes
        defaultMain $
          -- (singles dir singleSizes) <>
          multiples dir multipleSizes

multiples :: FilePath
           -> [Int]
           -> [Benchmark]
multiples dir names = [bgroup "multiplefiles"
  [ bgroup "archive"
             [
               bench "codec"       $ nfIO $ zipCodecMultiple dir $ show <$> names
             , bench "zip"         $ nfIO $ zipZipMultiple dir $ show <$> names
             ]
  , bgroup "unarchive"
             [
               bench "codec"       $ nfIO $ unZipCodecMultiple dir $ foldMap show names
             , bench "zip"         $ nfIO $ unZipZipMultiple dir $ foldMap show names
             ]
  , bgroup "async-unarchive"
  -- cretrion doesn't interfere apparantly with this https://www.reddit.com/r/haskell/comments/2m0pv6/force_criterion_to_benchmark_sequentially_for/
             [
               bench "codec"       $ nfIO $ unZipCodecMultipleAsync dir $ foldMap show names
             , bench "zip"         $ nfIO $ unZipZipMultipleAsync dir $ foldMap show names
             ]
  ]
  ]

-- | Prepares benchmarks.
singles :: FilePath     -- ^ the path to the directory with files
             -> [Int]        -- ^ sizes
             -> [Benchmark]
singles dir names = [bgroup "singlefiles"
    [ bgroup "archive"
             [
               bgroup "zip-archive" $ makeBenchmark dir names zipArchive
             -- , bgroup "libZip"      $ b libZip
             , bgroup "codec"       $ makeBenchmark dir names zipCodec
             , bgroup "zip"         $ makeBenchmark dir names zipZip
             , bgroup "zip-stream"  $ makeBenchmark dir names zipStream
             ]
    , bgroup "unarchive"
             [
               bgroup "zip-archive" $ makeBenchmark dir names unZipArchive
             , bgroup "zip-codec"   $ makeBenchmark dir names unZipCodec
             , bgroup "zip-zip"     $ makeBenchmark dir names unZipZip
             , bgroup "zip-stream"  $ makeBenchmark dir names unZipStream
             -- , bgroup "libZip"      $ b unLibZip -- way slower
             ]
    ]]

makeBenchmark :: FilePath -> [Int] -> (FilePath -> FilePath -> IO ()) -> [Benchmark]
makeBenchmark dir sizes benchFun = map (\size ->
                                          bench (Byte.getShortHand $ Byte.getAppropriateUnits $ Byte.ByteValue (fromIntegral size) Byte.Bytes) $
                                            nfIO $ benchFun dir $ show size
                                       ) sizes


-- | Creates source files for archiving and archives with those
-- files. File name is the size of this file in bytes.
prepareFiles :: FilePath    -- ^ the path to the directory for files
             -> [Int]       -- ^ sizes of files to create
             -> IO ()
prepareFiles dir sizes = do
  forM_ sizes $ \s -> do
    let path = dir </> show s

    createFile path s
    This.writeZipFile (path <.> "zip") $ Map.singleton "somename" $ This.readFileContent path

  let path = dir </> fold names
      names = show <$> sizes

  -- for some reason zip package can't find files within a zip file within a subfolder.
  This.writeZipFile (path <.> "zip") $ foldMap (\key -> Map.singleton key $ This.readFileContent $ dir </> key) names

-- | Creates a file of specified length with random content.
createFile :: FilePath -> Int -> IO ()
createFile path size =
    withFile path WriteMode $ \h -> do
        g <- getStdGen
        B.hPut h $ B.pack $ take size (randoms g :: [Word8])


------------------------------------------------------------------------------
-- Create zip archive with three different packages.

zipArchive :: FilePath -> FilePath -> IO ()
zipArchive dir name =
    withTempDirectory dir "zip-archive" $ \tmpDir -> do
        ar' <- A.addFilesToArchive [] A.emptyArchive [dir </> name]
        withFile (tmpDir </> name <.> "zip") WriteMode $ \h ->
            B.hPut h $ A.fromArchive ar'


libZip :: FilePath -> FilePath -> IO ()
libZip dir name =
    withTempDirectory dir "libZip" $ \tmpDir ->
        L.withArchive [L.CreateFlag] (tmpDir </> name <.> "zip") $ do
           zs <- L.sourceFile (dir </> name) 0 0
           _ <- L.addFile (dir </> name) zs
           return ()

zipCodec :: FilePath -> FilePath -> IO ()
zipCodec dir name = do
    withTempDirectory dir "codec" $ \tmpDir ->
      This.writeZipFile (tmpDir </> name <.> "zip") $
        This.readFileContentMap (dir </> name)

zipZip :: FilePath -> FilePath -> IO ()
zipZip dir name = do
    withTempDirectory dir "zip" $ \tmpDir ->
      Zip.createArchive (tmpDir </> name <.> "zip") $ do
        selector <- Zip.mkEntrySelector "x"
        Zip.loadEntry Zip.Deflate selector (dir </> name)

zipStream :: FilePath -> FilePath -> IO ()
zipStream dir name = do
    withTempDirectory dir "zip" $ \tmpDir ->
      runConduitRes $ yield (Stream.ZipEntry (Left (Text.pack name)) (LocalTime (toEnum 0) midnight) Nothing Nothing
                          ,  Stream.zipFileData (dir </> name))
                    .| void (Stream.zipStream Stream.defaultZipOptions)
                    .| CC.sinkFile (tmpDir </> name <.> "zip")

zipCodecMultiple :: FilePath -> [FilePath] -> IO ()
zipCodecMultiple dir names = do
    withTempDirectory dir "codec" $ \tmpDir ->
      This.writeZipFile (tmpDir </> fold names <.> "zip") $
        foldMap (\name -> This.readFileContentMap (dir </> name)) names

zipZipMultiple :: FilePath -> [FilePath] -> IO ()
zipZipMultiple dir names = do
    withTempDirectory dir "zip" $ \tmpDir ->
      Zip.createArchive (tmpDir </> fold names <.> "zip") $ do
        forM_ names $ \name -> do
          selector <- Zip.mkEntrySelector name
          Zip.loadEntry Zip.Deflate selector (dir </> name)

------------------------------------------------------------------------------
-- Exctract files from archive with three different packages.

unZipArchive :: FilePath -> FilePath -> IO ()
unZipArchive dir name = do
    bytes <- B.readFile (dir </> name <.> "zip")
    withCurrentDirectory dir . A.extractFilesFromArchive [] $ A.toArchive bytes


unLibZip :: FilePath -> FilePath -> IO ()
unLibZip dir name = do
    bytes <- L.withArchive [] (dir </> name <.> "zip") $ L.fileContentsIx [] 0
    withFile (dir </> name) WriteMode $ \h ->
        hPutStr h bytes

unZipCodec :: FilePath -> FilePath -> IO ()
unZipCodec dir name = do
    hashmapE <- This.readZipFile $ dir </> name <.> "zip"
    case hashmapE of
      Left x -> throwIO x
      Right map' -> forM_ map' $ \fileContent ->
        runConduitRes $ This.fcFileContents fileContent .| CC.sinkFile (dir </> name)

unZipZip :: FilePath -> FilePath -> IO ()
unZipZip dir name = do
  entry <- Zip.withArchive (dir </> name <.> "zip") $ do
    entries <- Zip.getEntries
    -- using conduit is much faster then reading it all into memory and
    -- then writing.
    Zip.getEntrySource $ fst $ head (Map.toList entries)
  runConduitRes $ entry.| CC.sinkFile (dir </> name)

unZipStream :: FilePath -> FilePath -> IO ()
unZipStream dir name =
  runConduitRes $ CC.sourceFile (dir </> name <.> "zip") .| void Stream.unZipStream
                                         .| CC.concatMap (\case
                                                               Right bs -> Just bs
                                                               Left _ -> Nothing)
                                         .| CC.sinkFile (dir </> name)

unZipCodecMultipleAsync :: FilePath -> FilePath -> IO ()
unZipCodecMultipleAsync = unZipCodecMultipleGeneral (Async.forConcurrently)

unZipCodecMultiple :: FilePath -> FilePath -> IO ()
unZipCodecMultiple = unZipCodecMultipleGeneral forM

unZipCodecMultipleGeneral :: (forall t a b. Traversable t => t a -> (a -> IO b) -> IO (t b)) -> FilePath -> FilePath -> IO ()
unZipCodecMultipleGeneral forFun dir name = do
    hashmapE <- This.readZipFile $ dir </> name <.> "zip"
    case hashmapE of
      Left x -> throwIO x
      Right map' -> do
        -- error $ show $ () <$ map'
        void $ forFun (imap (\k v -> (k,v)) map') $ \(key, fileContent) ->
          runConduitRes $ This.fcFileContents fileContent .| CC.sinkFile (dir </> key)

deriving instance Show Zip.EntryDescription

unZipZipMultipleAsync :: FilePath -> FilePath -> IO ()
unZipZipMultipleAsync = unZipZipMultipleGeneral Async.mapConcurrently

unZipZipMultiple :: FilePath -> FilePath -> IO ()
unZipZipMultiple = unZipZipMultipleGeneral traverse

unZipZipMultipleGeneral :: (forall t a b. Traversable t => (a -> IO b) -> t a -> IO (t b)) -> FilePath -> FilePath -> IO ()
unZipZipMultipleGeneral traverseFun dir name = do
  entryMap <- Zip.withArchive (dir </> name <.> "zip") $ do
    entries <- Zip.getEntries
    -- using conduit is much faster then reading it all into memory and
    -- then writing.
    itraverse (\key _ -> (key,) <$> Zip.getEntrySource key) entries

  void $ traverseFun (\(key, val) ->
                runConduitRes $ val .| CC.sinkFile (dir </> Zip.unEntrySelector key)
               ) entryMap

------------------------------------------------------------------------------
-- Utils.

-- | Runs action in the specified current directory.
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path action = withSystemTempDirectory path $ \dir -> do
    current <- getCurrentDirectory
    setCurrentDirectory dir
    res <- action
    setCurrentDirectory current
    return res
