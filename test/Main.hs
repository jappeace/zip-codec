{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           System.IO.Temp (withSystemTempDirectory)
import Zip.Codec.OSFile
import Data.Foldable
import qualified Data.ByteString.Lazy as B
import           Data.Word (Word8)
import           System.IO (IOMode(..), withFile)
import           System.Random (getStdGen, randoms)
import qualified Control.Concurrent.Async as Async
import Zip.Codec.DataDescriptor
import Zip.Codec.Time
import Zip.Codec.End
import Data.Text.Encoding(encodeUtf8)
import Control.Monad
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Zip.Codec.CentralDirectory
import Data.Serialize.Get
import Data.Serialize.Put
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map
import           System.FilePath ((</>), (<.>))
import Control.Exception
import Control.Monad.Trans.Resource
import Zip.Codec
import Control.Lens hiding ((<.>))
import Zip.Codec.FileHeader
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Instances()
import qualified Data.Map.Merge.Lazy as Map
import qualified "zip-archive" Codec.Archive.Zip as A

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Unit tests"
                [
                  testCase "file headers same" assertFileHeadersSame
                , testCase "file content same" assertFileContenTheSame
                , testCase "golden central dir same" assertCentralDirSame
                , testCase "golden against archive" unZipArchive
                , testCase "file content same async" assertFileContenTheSameAsync
                , testCase "reads golden somezip" assertsReadsGoldenSomeZip
                , testCase "cocnat many test 3" $ concatManyGeneric 3
                , testCase "cocnat many test 2" $ concatManyGeneric 2
                , testCase "cocnat many test 4" $ concatManyGeneric 4
                , testCase "cocnat many test 5" $ concatManyGeneric 5
                -- , testCase "conduit uncompressed" (assertFileHeadersSame sinkEntryUncompressed)
                -- -- , testCase "conduit parallel" (assertFileHeadersSame sinkEntryUncompressed)
                -- , testCase "files      " (assertFiles Nothing)
                -- , testCase "files-as   " (assertFiles $ Just ("contents" </>))

                , testCase "see if it can handle empty string" readCentralDir
                , testCase "see if file header can read" readFileHeader
                , QC.testProperty "endRoundTrip" endRoundTrip
                , QC.testProperty "fileHeaderRoundTrip" fileHeaderRoundTrip
                , QC.testProperty "centralDirectoryRoundTrip" centralDirectoryRoundTrip
                , QC.testProperty "dataDescriptorRoundTrip" dataDescriptorRoundTrip
                ]

-- this tests if we can decode a zip file made by another program
assertsReadsGoldenSomeZip :: IO ()
assertsReadsGoldenSomeZip = do
      result' <- readZipFile @(ResourceT IO) "test/somezip.zip"
      case result' of
          Left errors ->
            throwIO errors
          Right result -> do
            assertEqual "list diff is same" ["somezip/", "somezip/filex", "somezip/filey", "somezip/filez"]
                                            (fst <$> Map.toList result)

            results <- forM (Map.toList result) $ \(_, econtent) -> runConduitRes $ fcFileContents econtent .| C.fold
            assertEqual "contents same" ["", "xxx\n", "yyyyyy\n", "zzzzzzzzzzzz\n"]
                                        results

instance Arbitrary GeneralPurposeBitflag where
  arbitrary  = MkGeneralPurposeBitflag <$> arbitrary

instance Arbitrary End where
  arbitrary = End <$> arbitrary <*> arbitrary <*> arbitrary <*> (encodeUtf8 <$> arbitrary)

endRoundTrip :: End -> Property
endRoundTrip end =
  let out = runGet getEnd (runPut (putEnd end))
  in
  counterexample ("got this output: \n " <> show out) $ out == Right end

instance Arbitrary DataDescriptor where
  arbitrary = DataDescriptor <$> arbitrary <*> arbitrary <*> arbitrary

dataDescriptorRoundTrip :: DataDescriptor -> Property
dataDescriptorRoundTrip dataDescriptor =
  let out = runGet getDataDescriptor (runPut (putDataDescriptor dataDescriptor))
  in
  counterexample ("got this output: \n " <> show out) $ out == Right dataDescriptor

instance Arbitrary CompressionMethod where
  arbitrary = QC.elements [minBound..maxBound]

instance Arbitrary MSDOSDateTime where
  arbitrary = MSDOSDateTime <$> arbitrary <*> arbitrary


instance Arbitrary FileHeader where
  arbitrary = do
    fhBitFlag                 <- arbitrary
    fhCompressionMethod       <- arbitrary
    fhLastModified            <- arbitrary
    fhDataDescriptor          <- arbitrary
    fhInternalFileAttributes  <- arbitrary
    fhExternalFileAttributes  <- arbitrary
    fhRelativeOffset          <- arbitrary
    fhFileName                <- arbitrary
    fhExtraField              <- arbitrary
    fhFileComment             <- arbitrary
    pure $ FileHeader {..}

fileHeaderRoundTrip :: FileHeader -> Property
fileHeaderRoundTrip fileHeader =
  let out = runGet getFileHeader (runPut (putFileHeader fileHeader))
  in
  counterexample ("got this output: \n " <> show out) $ out == Right (Right fileHeader)

instance Arbitrary CentralDirectory where
  arbitrary = CentralDirectory .
    Map.mapWithKey (\key val -> val{ fhFileName = key} )
    <$>
    arbitrary

centralDirectoryRoundTrip :: CentralDirectory -> Property
centralDirectoryRoundTrip centralDirectory =
  let out = runGet getCentralDirectory (runPut (putCentralDirectory centralDirectory))
  in
  counterexample ("got this output: \n " <> show out) $ out == Right (Right centralDirectory)


-- this caused segaults in ghc runtime before, see https://github.com/GaloisInc/cereal/issues/105
readCentralDir :: IO ()
readCentralDir = do
  assertEqual "list diff is same" (Right (Right (CentralDirectory {cdFileHeaders = mempty}))) (runGet getCentralDirectory mempty)

readFileHeader :: IO ()
readFileHeader =
  assertEqual "file header same" (Left "too few bytes\nFrom:\tdemandInput\n\n") (runGet getFileHeader mempty)

-- TODO covnert into property tests
assertFileHeadersSame :: IO ()
assertFileHeadersSame = do
    withSystemTempDirectory "zip-conduit" $ \dir -> do
        let archivePath = dir </> archiveName
        _ <- writeZipFile archivePath $ Map.fromList entriesInfo
        result' <- readZipFile @(ResourceT IO) archivePath
        case result' of
          Left errors ->
            throwIO errors
          Right result ->
            assertEqual "list diff is same" (over (mapped . _2) fcFileHeader entriesInfo)
                                            ((over (mapped . _2) fcFileHeader $ Map.toList result))
  where
    archiveName = "test.zip"
    entriesInfo :: [(FilePath, FileContent (ResourceT IO))]
    entriesInfo = [ ("test1.txt", appendBytestring "some test text" defOptions)
                  , ("test2.txt", appendBytestring "some another test text" defOptions)
                  , ("test3.txt", appendBytestring "one more" defOptions)
                  ]


-- this may seem a little dumb but if you're going to refactor the format this better checks out
-- naturaly if adding fields to  fileheader it's merely a case of updating it.
-- so this may fail a lot, but if you didn't intend to update it it's a nice warning.
assertCentralDirSame :: IO ()
assertCentralDirSame = do
    withSystemTempDirectory "zip-conduit" $ \dir -> do
      writeZipFile (dir </> "somezip.zip") $ Map.fromList entriesInfo
      res <- readEndAndCentralDir (dir </> "somezip.zip")
      assertEqual "golden central dir same" golden res

    where

      entriesInfo :: [(FilePath, FileContent (ResourceT IO))]
      entriesInfo = [ ("test1.txt", appendBytestring "some test text" defOptions)
                  , ("test2.txt", appendBytestring "some another test text" defOptions)
                  , ("test3.txt", appendBytestring "one more" defOptions)
                  ]

      golden = Right (End {
                         endEntriesCount = 3, endCentralDirectorySize = 165, endCentralDirectoryOffset = 163 + 3*16, endZipComment = ""},
                      CentralDirectory {cdFileHeaders =
                                        Map.fromList [("test1.txt",FileHeader {fhBitFlag = 10, fhCompressionMethod = Deflate, fhLastModified = MSDOSDateTime {msDOSDate = 3441, msDOSTime = 0}, fhDataDescriptor = DataDescriptor {ddCRC32 = 359422662, ddCompressedSize = 14, ddUncompressedSize = 14}, fhInternalFileAttributes = 0, fhExternalFileAttributes = 0, fhRelativeOffset = 0, fhFileName = "test1.txt", fhExtraField = "", fhFileComment = ""}),
                                                      ("test2.txt",FileHeader {fhBitFlag = 10, fhCompressionMethod = Deflate, fhLastModified = MSDOSDateTime {msDOSDate = 3441, msDOSTime = 0}, fhDataDescriptor = DataDescriptor {ddCRC32 = 1656754388, ddCompressedSize = 22, ddUncompressedSize = 22}, fhInternalFileAttributes = 0, fhExternalFileAttributes = 0, fhRelativeOffset = 69, fhFileName = "test2.txt", fhExtraField = "", fhFileComment = ""}),
                                                      ("test3.txt",FileHeader {fhBitFlag = 10, fhCompressionMethod = Deflate, fhLastModified = MSDOSDateTime {msDOSDate = 3441, msDOSTime = 0}, fhDataDescriptor = DataDescriptor {ddCRC32 = 2223949387, ddCompressedSize = 10, ddUncompressedSize = 8}, fhInternalFileAttributes = 0, fhExternalFileAttributes = 0, fhRelativeOffset = 146, fhFileName = "test3.txt", fhExtraField = "", fhFileComment = ""})
                                                     ]})

assertFileContenTheSame :: IO ()
assertFileContenTheSame = assertFileContenTheSameGeneral forM

assertFileContenTheSameAsync :: IO ()
assertFileContenTheSameAsync = assertFileContenTheSameGeneral Async.forConcurrently

assertFileContenTheSameGeneral :: (forall t a b. Traversable t => t a -> (a -> IO b) -> IO (t b))  -> IO ()
assertFileContenTheSameGeneral forFun = do
    withSystemTempDirectory "zip-conduit" $ \dir -> do
        let entriesInfo = entriesInfoFun dir
            entriesInfoMap = Map.fromList entriesInfo
            archivePath = dir </> archiveName
        createFile (dir </> "bigly1.file") $ 1024 * 1024 * 1
        createFile (dir </> "bigly2.file") $ 1024 * 1024 * 1
        createFile (dir </> "bigly3.file") $ 1024 * 1024 * 2
        createFile (dir </> "bigly4.file") $ 1024 * 1024 * 1
        createFile (dir </> "bigly5.file") $ 1024 * 1024 * 3
        _ <- writeZipFile archivePath $ Map.fromList entriesInfo
        result' <- readZipFile @(ResourceT IO) archivePath
        case result' of
          Left errors ->
            throwIO errors
          Right result -> do
            assertEqual "length same" (length entriesInfo) (length result)
            let merged = Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMatched (\k -> (k,,)))
                                entriesInfoMap result


            void $ forFun merged $ \(fileName, econtent, fcontent) -> do
                expected  <- runConduitRes $ fcFileContents econtent .| C.fold
                resulted <- runConduitRes $ fcFileContents fcontent .| C.fold
                assertEqual ("same content" <> fileName) expected resulted
  where
    archiveName = "test.zip"
    entriesInfoFun :: FilePath -> [(FilePath, FileContent (ResourceT IO))]
    entriesInfoFun dir = [ ("test1.txt", appendBytestring "some test text" defOptions)
                  , ("test2.txt", appendBytestring "some another test text" defOptions)
                  , ("test3.txt", appendBytestring "one more" defOptions)
                  , ("bigly", readFileContent (dir </> "bigly1.file" ))
                  , ("bigly2", readFileContent (dir </> "bigly2.file" ))
                  , ("bigly3", readFileContent (dir </> "bigly3.file" ))
                  , ("bigly4", readFileContent (dir </> "bigly4.file" ))
                  , ("bigly5", readFileContent (dir </> "bigly5.file" ))
                  ]

createFile :: FilePath -> Int -> IO B.ByteString
createFile path size =
    withFile path WriteMode $ \h -> do
        g <- getStdGen
        let content = B.pack $ take size (randoms g :: [Word8])
        content <$ (B.hPut h content)

concatManyGeneric :: Int -> IO ()
concatManyGeneric size = do
    withSystemTempDirectory ("concat-many-" <> show size) $ \dir -> do
      let filepaths = (dir </>) . show <$> ([1..size] :: [Int])
      expected <- traverse (\x -> createFile x 10) filepaths
      res <- concatManyAsync filepaths
      fileRes <- B.readFile res
      assertEqual "concat produced a file that has same content" (fold expected) fileRes

unZipArchive :: IO ()
unZipArchive = do
    withSystemTempDirectory ("archive-implementation") $ \dir -> do
      let name = "xyz"
          path = dir </> name
      createFile path 100
      writeZipFile (path <.> "zip") $ Map.singleton "somename" $ readFileContent path
      bytes <- B.readFile (dir </> name <.> "zip")
      A.extractFilesFromArchive [] $ A.toArchive bytes
