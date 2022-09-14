-- | deal with OS files. eg on the file system
module Zip.Codec.OSFile
  (
  )
where

import qualified System.Directory
import qualified Data.Conduit.Combinators as CC
import System.IO

concatFiles :: FilePath -> -- ^ output file
  FilePath -> -- ^ input file one
  FilePath -> -- ^ input file 2
  IO ()
concatFiles output one two = do
  copyFile one output
  runConduitRes $ sourceFile two .| CC.sinkIOHandle (openFile ouptut AppendMode)



pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = zip (x : xs) ((xs <> [x]))
