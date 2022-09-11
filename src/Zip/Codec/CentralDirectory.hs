{-# LANGUAGE ScopedTypeVariables #-}

module Zip.Codec.CentralDirectory
  ( CentralDirectory(..)
  , readCentralDirectory
  , getCentralDirectory
  , CenteralDirErrors(..)
  , writeCentralDirectory
  , putCentralDirectory
  )
where

import Zip.Codec.FileHeader
import Data.Map
import Data.Serialize.Get
import Data.Serialize.Put
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           System.IO (Handle, SeekMode(..),  hSeek)
import Zip.Codec.End
import Control.Applicative(many)
import Data.Maybe(catMaybes)
import Data.Functor((<&>))
import Data.Bifunctor


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

data CenteralDirErrors = MkGetErrors String
                       | FileDecodeErrors GetFileHeaderError
                       deriving Show

readCentralDirectory :: Handle -> End -> IO (Either CenteralDirErrors CentralDirectory)
readCentralDirectory h e = do
    bs <- hGetCentralDirectory h e
    pure $
      first FileDecodeErrors =<< (first MkGetErrors $ runGet getCentralDirectory bs)

getCentralDirectory :: Get (Either GetFileHeaderError CentralDirectory)
getCentralDirectory = do
    headers' :: [Maybe (Either GetFileHeaderError FileHeader)] <- many $ maybeEmpty $ getFileHeader

    -- TODO collect all decode errors rather then squashing
    let headersE :: Either GetFileHeaderError [FileHeader]
        headersE = sequence $ catMaybes headers'
    return $ headersE <&> \headers ->
      CentralDirectory { cdFileHeaders =
                         fromList $ (\header -> (fhFileName header, header)) <$> headers }

hGetCentralDirectory :: Handle -> End -> IO ByteString
hGetCentralDirectory h e = do
    hSeek h AbsoluteSeek $ fromIntegral offset
    B.hGet h size'
  where
    size'  = endCentralDirectorySize e
    offset = endCentralDirectoryOffset e

maybeEmpty :: Get a -> Get (Maybe a)
maybeEmpty p = do
    e <- isEmpty
    if e
      then return Nothing
      else Just <$> p

writeCentralDirectory :: Handle -> CentralDirectory -> IO ()
writeCentralDirectory h cd =
    B.hPut h . runPut $ putCentralDirectory cd


putCentralDirectory :: CentralDirectory -> Put
putCentralDirectory cd =
    mapM_ putFileHeader $ cdFileHeaders cd
