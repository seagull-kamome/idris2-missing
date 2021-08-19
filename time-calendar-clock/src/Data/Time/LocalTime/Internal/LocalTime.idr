module Data.Time.LocalTime.Internal.LocalTime

import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Clock.DiffTime
import Data.Time.Clock.Internal.UTCTime
import Data.Time.LocalTime.TimeZone
import Data.Time.LocalTime.TimeOfDay

%default total

-- --------------------------------------------------------------------------

||| A simple day and time aggregate, where the day is of the specified parameter,
||| and the time is a TimeOfDay.
||| Conversion of this (as local civil time) to UTC depends on the time zone.
||| Conversion of this (as local mean time) to UT1 depends on the longitude.
public export
record LocalTime where
  constructor MkLocalTime
  localDay : Day
  localTimeOfDay : TimeOfDay

public export
Eq LocalTime where
  x == y = x.localDay == y.localDay && x.localTimeOfDay == y.localTimeOfDay

public export
Ord LocalTime where
  compare x y =
    case compare x.localDay y.localDay of
         EQ => compare x.localTimeOfDay y.localTimeOfDay
         x  => x

public export
Show LocalTime where
  show x = show x.localDay ++ " " ++ show x.localTimeOfDay

-- --------------------------------------------------------------------------

||| Get the local time of a UTC time in a time zone.
export utcToLocalTime : TimeZone -> UTCTime -> LocalTime
utcToLocalTime tz (MkUTCTime day dt) = let
  (i, tod) = utcToLocalTimeOfDay tz (timeToTimeOfDay dt)
  in MkLocalTime (addDays i day) tod

||| Get the UTC time of a local time in a time zone.
export localTimeToUTC : TimeZone -> LocalTime -> UTCTime
localTimeToUTC tz (MkLocalTime day tod) = let
  (i, todUTC) = localToUTCTimeOfDay tz tod
  in MkUTCTime (addDays i day) (timeOfDayToTime todUTC)



-- vim: tw=80 sw=2 expandtab :
