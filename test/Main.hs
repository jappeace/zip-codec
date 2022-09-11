module Main where

import Zip.Codec.CentralDirectory
import Data.Serialize.Get
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map
import System.IO.Temp(withSystemTempDirectory)
import           System.FilePath ((</>))
import Control.Exception
import           Data.List ((\\))
import Control.Monad.Trans.Resource
import Zip.Codec
import Control.Lens
import Zip.Codec.FileHeader
import Control.Applicative(many)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Unit tests"
                [
                  testCase "file headers same" assertFileHeadersSame
                -- , testCase "conduit uncompressed" (assertFileHeadersSame sinkEntryUncompressed)
                -- -- , testCase "conduit parallel" (assertFileHeadersSame sinkEntryUncompressed)
                -- , testCase "files      " (assertFiles Nothing)
                -- , testCase "files-as   " (assertFiles $ Just ("contents" </>))

                , testCase "see if it can handle empty string" readCentralDir
                , testCase "see if file header can read" readFileHeader
                ]

-- TODO property tests for all get/puts


-- this caused segaults in ghc runtime before, see https://github.com/GaloisInc/cereal/issues/105
readCentralDir :: IO ()
readCentralDir = do
  assertEqual "list diff is same" (Right (Right (CentralDirectory {cdFileHeaders = mempty}))) (runGet getCentralDirectory mempty)

readFileHeader :: IO ()
readFileHeader =
  assertEqual "file header same" (Left "too few bytes\nFrom:\tdemandInput\n\n") (runGet getFileHeader mempty)

assertFileHeadersSame :: IO ()
assertFileHeadersSame = do
    putStrLn "wnetering"
    withSystemTempDirectory "zip-conduit" $ \dir -> do
        let archivePath = dir </> archiveName

        putStrLn "writing zip file"
        _ <- writeZipFile archivePath $ Map.fromList entriesInfo
        putStrLn "reading zip file"
        result' <- readZipFile @(ResourceT IO) archivePath
        putStrLn "read zip file"
        case result' of
          Left errors -> do
            putStrLn "dying"
            throwIO errors
          Right result -> do
            putStrLn "asserting equal"
            assertEqual "list diff is same" [] ((over (mapped . _2) fcFileHeader entriesInfo) \\
                                                (over (mapped . _2) fcFileHeader $ Map.toList result))
  where
    archiveName = "test.zip"
    entriesInfo :: [(FilePath, FileContent (ResourceT IO))]
    entriesInfo = [ ("test1.txt", appendBytestring "some test text" defOptions)
                  , ("test2.txt", appendBytestring "some another test text" defOptions)
                  , ("test3.txt", appendBytestring "one more" defOptions)
                  ]
