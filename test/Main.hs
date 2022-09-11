module Main where

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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Unit tests"
                [ testCase "file headers same" assertFileHeadersSame
                -- , testCase "conduit uncompressed" (assertFileHeadersSame sinkEntryUncompressed)
                -- -- , testCase "conduit parallel" (assertFileHeadersSame sinkEntryUncompressed)
                -- , testCase "files      " (assertFiles Nothing)
                -- , testCase "files-as   " (assertFiles $ Just ("contents" </>))
                ]

-- TODO property tests for all get/puts

assertFileHeadersSame :: IO ()
assertFileHeadersSame =
    withSystemTempDirectory "zip-conduit" $ \dir -> do
        let archivePath = dir </> archiveName

        writeZipFile archivePath $ Map.fromList entriesInfo
        result' <- readZipFile @(ResourceT IO) archivePath
        case result' of
          Left errors -> throwIO errors
          Right result -> do
            assertEqual "list diff is same" [] ((over (mapped . _2) fcFileHeader entriesInfo) \\
                                                (over (mapped . _2) fcFileHeader $ Map.toList result))
  where
    archiveName = "test.zip"
    entriesInfo :: [(FilePath, FileContent (ResourceT IO))]
    entriesInfo = [ ("test1.txt", appendBytestring "some test text" defOptions)
                  , ("test2.txt", appendBytestring "some another test text" defOptions)
                  , ("test3.txt", appendBytestring "one more" defOptions)
                  ]
