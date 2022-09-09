-- | A wrapper around data.binary.get which puts the tuple into a record.
module Zip.Codec.Get
  ( runGet
  , runGetUnconsumed
  , GetResult(..)
  )
where

import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as L
import Data.Bifunctor

data GetResult a = MkGetResult
  { getUnconsumed :: L.ByteString -- ^  any unconsumed input
  , getConsumedCount :: Get.ByteOffset -- ^  the number of bytes consumed
  , getContent :: a
  }

tupleToGetResult :: (L.ByteString, Get.ByteOffset, a) -> GetResult a
tupleToGetResult (a,b,c) = MkGetResult a b c

runGet :: Get.Get a -> L.ByteString -> Either (GetResult String) a
runGet bm bs =
  fmap getContent $
  runGetUnconsumed bm bs

runGetUnconsumed :: Get.Get a -> L.ByteString -> Either (GetResult String) (GetResult a)
runGetUnconsumed get bs =
  (bimap tupleToGetResult tupleToGetResult) $
  Get.runGetOrFail get bs
