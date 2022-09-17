-- | deal with OS files. eg on the file system
module Zip.Codec.OSFile
  ( concatFilesInPlace
  , concatManyAsync
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Data.Conduit.Combinators as CC
import System.IO
import Data.Conduit

data FileConcat = MkFileConcat {
    writenInto :: FilePath -- ^ the file we're appending readFrom to
  , readFrom :: FilePath
  }

-- | append the second file to the first file
concatFilesInPlace :: FilePath -> FilePath -> IO FileConcat
concatFilesInPlace one two = do
  MkFileConcat {writenInto = one, readFrom = two} <$
    runConduitRes (CC.sourceFile two .| CC.sinkIOHandle (openFile one AppendMode))

-- | this will concat files asyncronusly and recursively
--   resulting in a single file
concatManyAsync :: [FilePath] -> IO FilePath
concatManyAsync [] = pure [] -- wtf? crap goes in, crap goes out I suppose
concatManyAsync allpaths@(apath:manypaths) =
  if length paths > 1 then do
  -- putStrLn "loop"
  concats <- Async.mapConcurrently (uncurry concatFilesInPlace) filtered
  let writtenInto = writenInto <$> concats -- this halves the pairing
  concatManyAsync $ case remainder of
    Just x -> x : writtenInto
    Nothing -> writtenInto
  else pure apath
  where
    -- we don't do the first one if we have an odd count,
    -- this prevents resource locking
    (remainder, paths) = if odd (length allpaths) then (Just apath, manypaths) else (Nothing, allpaths)

    paired :: [((FilePath, FilePath), Int)]
    paired = zip (pairs paths) numbers


    filtered = fmap fst $ filter (\(_, num) -> even num) paired

    numbers :: [Int]
    numbers = [0..]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = zip (x : xs) ((xs <> [x]))
