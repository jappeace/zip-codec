{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

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
import System.IO.Temp(withSystemTempDirectory)
import           System.FilePath ((</>))
import Control.Exception
import Control.Monad.Trans.Resource
import Zip.Codec
import Control.Lens
import Zip.Codec.FileHeader
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Instances()

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Unit tests"
                [
                  testCase "file headers same" assertFileHeadersSame
                , testCase "file content same" assertFileContenTheSame
                , testCase "reads golden somezip" assertsReadsGoldenSomeZip
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

assertFileContenTheSame :: IO ()
assertFileContenTheSame = do
    withSystemTempDirectory "zip-conduit" $ \dir -> do
        let archivePath = dir </> archiveName
        _ <- writeZipFile archivePath $ Map.fromList entriesInfo
        result' <- readZipFile @(ResourceT IO) archivePath
        case result' of
          Left errors ->
            throwIO errors
          Right result -> do
            assertEqual "length same" (length entriesInfo) (length result)
            void $ forM (zip entriesInfo (Map.toList result)) $ \((efileName, econtent), (_rfileName, fcontent)) -> do
                expected  <- runConduitRes $ fcFileContents econtent .| C.fold
                resulted <- runConduitRes $ fcFileContents fcontent .| C.fold
                assertEqual ("same content" <> efileName) expected resulted
  where
    archiveName = "test.zip"
    entriesInfo :: [(FilePath, FileContent (ResourceT IO))]
    entriesInfo = [ ("test1.txt", appendBytestring "some test text" defOptions)
                  , ("test2.txt", appendBytestring "some another test text" defOptions)
                  , ("test3.txt", appendBytestring "one more" defOptions)
                  ]
