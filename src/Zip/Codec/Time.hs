-- | zip related time odds and ends
module Zip.Codec.Time
  ( MSDOSDateTime(..)
  , msDOSDateTimeToUTCTime
  , utcTimeToMSDOSDateTime
  , clockTimeToUTCTime
  )
where

import           Data.Time (UTCTime(..), TimeOfDay(..), fromGregorian, picosecondsToDiffTime, secondsToDiffTime, timeToTimeOfDay, toGregorian)
import           System.Time (ClockTime(..))
import Data.Word
import           Data.Bits ((.&.), shiftR, shiftL)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

data MSDOSDateTime = MSDOSDateTime
    { msDOSDate :: Word16
    , msDOSTime :: Word16
    } deriving (Show, Eq)


-- | convert a utc time to msdosdate time
msDOSDateTimeToUTCTime :: MSDOSDateTime -> UTCTime
msDOSDateTimeToUTCTime dosDateTime =
    UTCTime { utctDay = fromGregorian year month day
            , utctDayTime =
                secondsToDiffTime $ hours * 60 * 60 + minutes * 60 + seconds
            }
  where
    seconds = fromIntegral $ 2 * (dosTime .&. 0x1F)
    minutes = fromIntegral $ (shiftR dosTime 5) .&. 0x3F
    hours   = fromIntegral $ shiftR dosTime 11

    day     = fromIntegral $ dosDate .&. 0x1F
    month   = fromIntegral $ (shiftR dosDate 5) .&. 0xF
    year    = 1980 + fromIntegral (shiftR dosDate 9)

    dosDate = msDOSDate dosDateTime
    dosTime = msDOSTime dosDateTime

-- | convert a utc time to msdosdate time, not that this silently
--   clamps the UTCTime to fit within the msdos ime
--   meaning for example we can't represent dates before 1986-11-17
utcTimeToMSDOSDateTime :: UTCTime -> MSDOSDateTime
utcTimeToMSDOSDateTime utcTime =
    MSDOSDateTime { msDOSDate = dosDate
                  , msDOSTime = dosTime
                  }
  where
    dosTime = fromIntegral $ seconds + shiftL minutes 5 + shiftL hours 11
    dosDate = fromIntegral $ day + shiftL month 5 + shiftL year 9

    seconds = fromEnum (todSec tod) `div` 2
    minutes = todMin tod
    hours   = todHour tod
    tod     = timeToTimeOfDay $ utctDayTime utcTime

    year    = fromIntegral year' - 1980
    (year', month, day) = toGregorian $ utctDay utcTime


clockTimeToUTCTime :: ClockTime -> UTCTime
clockTimeToUTCTime (TOD seconds picoseconds) =
    let utcTime = posixSecondsToUTCTime $ fromIntegral seconds in
    utcTime { utctDayTime = utctDayTime utcTime
                          + picosecondsToDiffTime picoseconds
            }
