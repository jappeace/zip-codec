module Zip.Codec.CentralDirectory
  ( CentralDirectory(..)
  , readCentralDirectory
  , getCentralDirectory
  )
where

import Zip.Codec.FileHeader
import Zip.Codec.Get
import Data.Map


-- | Central directory structure:
--
-- [file header 1]
-- ...
-- [file header n]
--
-- we represent it as a map internally.
data CentralDirectory = CentralDirectory
    -- a hashmap maybe faster but it opens up a DoS vulnrability
    -- due to fnv being vulnrable to universal collisions.
    { cdFileHeaders      :: Map FilePath FileHeader -- this representation will filter out double files for better or worse.
    } deriving (Show)


readCentralDirectory :: Handle -> End -> IO (Either (GetResult String) CentralDirectory)
readCentralDirectory h e =
    runGet getCentralDirectory <$> hGetCentralDirectory h e

getCentralDirectory :: Get CentralDirectory
getCentralDirectory = do
    headers <- many . maybeEmpty $ getFileHeader

    return CentralDirectory { cdFileHeaders = fromListWith (\header -> fhFileName header) headers }

hGetCentralDirectory :: Handle -> End -> IO ByteString
hGetCentralDirectory h e = do
    hSeek h AbsoluteSeek $ fromIntegral offset
    B.hGet h size
  where
    size   = endCentralDirectorySize e
    offset = endCentralDirectoryOffset e

maybeEmpty :: Get a -> Get (Maybe a)
maybeEmpty p = do
    e <- isEmpty
    if e
      then return Nothing
      else Just <$> p
