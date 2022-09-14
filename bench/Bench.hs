{-# LANGUAGE PackageImports #-}

module Main where

import Data.Foldable(fold)
import qualified Data.Map as Map
import           Control.Monad (forM_, void)
import qualified Data.ByteString.Lazy as B (hPut, pack, readFile)
import           Data.Word (Word8)
import           System.FilePath ((</>), (<.>))
import           System.IO (IOMode(..), hPutStr, withFile)
import           System.Directory (getCurrentDirectory, setCurrentDirectory)

import           Criterion.Main (Benchmark, bench, bgroup, defaultMain, nfIO)
import           System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import           System.Random (getStdGen, randoms)

import Data.Traversable.WithIndex
import Control.Exception
import qualified Data.Conduit.Combinators as CC
import Data.Conduit ((.|), runConduitRes)
import qualified Codec.Archive.LibZip as L
import qualified "zip-archive" Codec.Archive.Zip as A
import qualified "zip" Codec.Archive.Zip as Zip

import qualified Zip.Codec as This
import qualified Data.ByteUnits as Byte


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
             --   bench "codec"       $ nfIO $ zipCodecMultiple dir $ show <$> names
             -- , bench "zip"         $ nfIO $ zipZipMultiple dir $ show <$> names
             ]
  , bgroup "unarchive"
             [
               bench "codec"       $ nfIO $ unZipCodecMultiple dir $ foldMap show names
             , bench "zip"         $ nfIO $ unZipZipMultiple dir $ foldMap show names
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
             ]
    , bgroup "unarchive"
             [
               bgroup "zip-archive" $ makeBenchmark dir names unZipArchive
             , bgroup "zip-codec"   $ makeBenchmark dir names unZipCodec
             , bgroup "zip-zip"     $ makeBenchmark dir names unZipZip
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


unZipCodecMultiple :: FilePath -> FilePath -> IO ()
unZipCodecMultiple dir name = do
    hashmapE <- This.readZipFile $ dir </> name <.> "zip"
    case hashmapE of
      Left x -> throwIO x
      Right map' -> do
        -- error $ show $ () <$ map'
        forM_ map' $ \fileContent ->
          runConduitRes $ This.fcFileContents fileContent .| CC.sinkFile (dir </> name)

deriving instance Show Zip.EntryDescription

unZipZipMultiple :: FilePath -> FilePath -> IO ()
unZipZipMultiple dir name = do
  entryMap <- Zip.withArchive (dir </> name <.> "zip") $ do
    entries <- Zip.getEntries
    -- using conduit is much faster then reading it all into memory and
    -- then writing.
    itraverse (\key _ -> Zip.getEntrySource key) entries

  void $ itraverse (\key val ->
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
