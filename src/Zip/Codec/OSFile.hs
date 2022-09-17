-- | deal with OS files. eg on the file system
module Zip.Codec.OSFile
  ( concatFilesInPlace
  , concatManyAsync
  )
where

import Control.Monad
import qualified Control.Concurrent.Async as Async
import System.Directory
import qualified Data.Conduit.Combinators as CC
import System.IO
import Data.Conduit

data FileConcat = MkFileConcat {
    writenInto :: FilePath -- ^ the file we're appending readFrom to
  , readFrom :: FilePath
  }

-- | Let's us figure out how to write the end
-- you'd say we could figure this out from input,
-- the issue is that the concurrent run makes
-- it uncertain whcih files get appended to what first
data AsyncResult = MkAsyncResult {
    asyncResultConcats :: [FileConcat]
  , asyncResultNextLayer :: Maybe AsyncResult
  }

emptyResult :: AsyncResult
emptyResult = MkAsyncResult [] Nothing

-- | append the second file to the first file
concatFilesInPlace :: FilePath -> FilePath -> IO FileConcat
concatFilesInPlace one two =
  FileConcat {writenInto = one, readFrom = two} <$
  runConduitRes (CC.sourceFile two .| CC.sinkIOHandle (openFile one AppendMode))

-- | this will concat files asyncronusly and recursively
--   resulting in a single file
concatManyAsync :: [FilePath] -> IO AsyncResult
concatManyAsync [] = pure emptyResult
concatManyAsync paths =
  if length paths > 1 then do
  concats <- Async.mapConcurrently (uncurry concatFilesInPlace) filtered
  manyRes <- concatManyAsync $ writenInto <$> concats
  -- nice spacy leaky, we'll fix it when people start bitchin'.
  pure $ MkAsyncResult {
      asyncResultConcats = concats
    , asyncResultNextLayer = Just manyRes
    }
  else pure emptyResult
  where
    paired :: [((FilePath, FilePath), Int)]
    paired = zip (pairs paths) numbers

    filtered = fmap fst $ filter (\(_, num) -> even num) paired

    numbers :: [Int]
    numbers = [0..]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = zip (x : xs) ((xs <> [x]))
