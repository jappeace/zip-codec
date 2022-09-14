{-# LANGUAGE PackageImports #-}

module Main where

import qualified Data.Map as Map
import           Control.Monad (forM_)
import qualified Data.ByteString.Lazy as B (hPut, pack, readFile)
import qualified Data.ByteString as BS
import           Data.Word (Word8)
import           System.FilePath ((</>), (<.>))
import           System.IO (IOMode(..), hPutStr, withFile)
import           System.Directory (getCurrentDirectory, setCurrentDirectory)

import           Criterion.Main (Benchmark, bench, bgroup, defaultMain, nfIO)
import           System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import           System.Random (getStdGen, randoms)

import Control.Exception
import qualified Data.Conduit.Combinators as CC
import Data.Conduit ((.|), runConduitRes)
import qualified Codec.Archive.LibZip as L
import qualified "zip-archive" Codec.Archive.Zip as A
import qualified "zip" Codec.Archive.Zip as Zip

import qualified Zip.Codec as This


main :: IO ()
main = do
    let sizes = [1024*1024, 10*1024*1024]    -- ^ sizes of files for benchmarking

    withSystemTempDirectory "zip-conduit" $ \dir -> do
        prepareFiles dir sizes
        defaultMain (prepareBench dir $ map show sizes)


-- | Prepares benchmarks.
prepareBench :: FilePath     -- ^ the path to the directory with files
             -> [FilePath]   -- ^ file names
             -> [Benchmark]
prepareBench dir names =
    [ bgroup "archive"
             [
             --   bgroup "zip-archive" $ b zipArchive
             -- , bgroup "libZip"      $ b libZip
             -- , bgroup "codec"       $ b zipCodec
             -- , bgroup "zip"         $ b zipZip
             ]
    , bgroup "unarchive"
             [
               bgroup "zip-archive" $ b unZipArchive
             , bgroup "zip-codec"   $ b unZipCodec
             , bgroup "zip-zip"     $ b unZipZip
             -- , bgroup "libZip"      $ b unLibZip -- way slower
             ]
    ]
  where
    b f = map (\name -> bench name $ nfIO $ f dir name) names


-- | Creates source files for archiving and archives with those
-- files. File name is the size of this file in bytes.
prepareFiles :: FilePath    -- ^ the path to the directory for files
             -> [Int]       -- ^ sizes of files to create
             -> IO ()
prepareFiles dir sizes = forM_ sizes $ \s -> do
    let path = dir </> show s

    createFile path s
    This.writeZipFile (path <.> "zip") $ Map.singleton "somename" $ This.readFileContent path


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
           L.addFile (dir </> name) zs
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
