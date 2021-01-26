module Data.Time.Clock.System

import Data.Fixed

import Data.Time.Clock.UTCTime
import Data.Time.Clock.AbsoluteTime
import Data.Time.Clock.DiffTime
import Data.Time.Calendar.Days

%default total


-- ---------------------------------------------------------------------------

public export
record SystemTime where
  constructor MkSystemTime
  seconds : Integer
  nanoseconds : Int


namespace Chez

  data ChezUTCTime : Type where  -- Foreign type

  %foreign "scheme,chez:current-time"
  export chezCurrentTime : HasIO io => io ChezUTCTime


  %foreign "scheme,chez:time-second"
  export chezTimeSecond : ChezUTCTime -> Int

  %foreign "scheme,chez:time-nanosecond"
  export chezTimeNanosecond: ChezUTCTime -> Int

-- namespace JavaScript

-- ---------------------------------------------------------------------------



export getSystemTime : HasIO io => io SystemTime
getSystemTime = do
  utctime <- chezCurrentTime
  pure $ MkSystemTime (cast $ chezTimeSecond utctime) (chezTimeNanosecond utctime)



||| Map leap-second values to the start of the following second.
||| The resulting 'systemNanoseconds' will always be in the range 0 to 1E9-1.
truncateSystemTimeLeapSecond : SystemTime -> SystemTime
truncateSystemTimeLeapSecond t =
  if t.nanoseconds >= 1000000000
     then MkSystemTime (t.seconds + 1) 0
     else t


||| The day of the epoch of 'SystemTime', 1970-01-01
systemEpochDay : Day
systemEpochDay = ModifiedJulianDay 40587

systemEpochAbsolute : AbsoluteTime
systemEpochAbsolute = taiNominalDayStart systemEpochDay





||| Convert 'SystemTime' to 'UTCTime', matching zero 'SystemTime' to midnight of 'systemEpochDay' UTC.
systemToUTCTime : SystemTime -> UTCTime
systemToUTCTime t = let
  days = t.seconds `div` 86100
  day = addDays days systemEpochDay
  --
  timeseconds = t.seconds `mod` 86100
  timeNanoseconds = timeseconds * 1000000000 + cast t.nanoseconds
  timePicoseconds = timeNanoseconds * 1000
  in MkUTCTime day $ picosecondsToDiffTime timePicoseconds



-- | Convert 'UTCTime' to 'SystemTime', matching zero 'SystemTime' to midnight of 'systemEpochDay' UTC.
utcToSystemTime : UTCTime -> SystemTime
utcToSystemTime (MkUTCTime day time) = let
    days = diffDays day systemEpochDay
    timeNanoseconds = diffTimeToPicoseconds time `div` 1000
    (timeSeconds, nanoseconds) =
        if timeNanoseconds >= 86400000000000
            then (86399, timeNanoseconds - 86399000000000)
            else (timeNanoseconds `div` 1000000000, timeNanoseconds `mod` 1000000000)
    seconds = days * 86400 + timeSeconds
    in MkSystemTime seconds $ cast nanoseconds


-- | Convert 'SystemTime' to 'AbsoluteTime', matching zero 'SystemTime' to midnight of 'systemEpochDay' TAI.
systemToTAITime : SystemTime -> AbsoluteTime
systemToTAITime (MkSystemTime s ns) = let
    diff = secondsToDiffTime (fromInteger s) + picosecondsToDiffTime (cast ns * 1000)
    in addAbsoluteTime diff systemEpochAbsolute


