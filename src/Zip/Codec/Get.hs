module Zip.Codec.Get
  ( runGet
  , runGetUnconsumed
  )
where

import qualified Data.Binary.Get as Get
import Data.ByteString.Lazy

data GetResult a = MkGetResult {
  getUnconsumed :: ByteString -- ^  any unconsumed input
  getConsumedCount :: Get.ByteOffset -- ^  the number of bytes consumed
  getContent :: a
  }

tupleToGetResult :: (L.ByteString, ByteOffset, a) -> GetResult a
tupleToGetResult (a,b,c) = MkGetResult a b c

runGet :: Get.Get a -> ByteString -> Either (GetResult String) a
runGet bm bs =
  fmap getContent .
  runGetUnconsumed


runGetUnconsumed :: Get.Get a -> ByteString -> Either (GetResult String) (GetResult a)
runGetUnconsumed =
  bimap tupleToGetResult tupleToGetResult .
  Get.runGetOrFail
