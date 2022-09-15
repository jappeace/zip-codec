-- | deal with OS files. eg on the file system
module Zip.Codec.OSFile
  ( concatFilesCopyNew
  , concatFilesInPlace
  , concatMany
  )
where

import Control.Monad
import qualified Control.Concurrent.Async as Async
import System.Directory
import qualified Data.Conduit.Combinators as CC
import System.IO
import Data.Conduit

-- | creates a new file with the content of the first and second
concatFilesCopyNew :: FilePath -> -- ^ output file
  FilePath -> -- ^ input file one
  FilePath -> -- ^ input file 2
  IO ()
concatFilesCopyNew output one two = do
  copyFile one output
  runConduitRes $ CC.sourceFile two .| CC.sinkIOHandle (openFile output AppendMode)

-- | append the second file to the first file
concatFilesInPlace :: FilePath -> FilePath -> IO ()
concatFilesInPlace one two =
  runConduitRes $ CC.sourceFile two .| CC.sinkIOHandle (openFile one AppendMode)

concatMany :: [FilePath] -> IO ()
concatMany paths =
  if length paths > 1 then do
  void $ Async.mapConcurrently (uncurry concatFilesInPlace) filtered
  concatMany $ fst <$> filtered
  else pure ()
  where
    paired :: [((FilePath, FilePath), Int)]
    paired = zip (pairs paths) numbers

    filtered = fmap fst $ filter (\(_, num) -> even num) paired

    numbers :: [Int]
    numbers = [0..]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = zip (x : xs) ((xs <> [x]))
