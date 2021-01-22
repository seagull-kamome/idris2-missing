module Data.Time.LocalTime

import Data.Rational

import public Data.Time.LocalTime.TimeZone
import public Data.Time.LocalTime.TimeOfDay
import public Data.Time.LocalTime.CalendarDiffTime

import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Clock.DiffTime
import Data.Time.Clock.UTCTime
import Data.Time.Clock

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


||| addLocalTime a b = a + b
export addLocalTime : NominalDiffTime -> LocalTime -> LocalTime
addLocalTime x = utcToLocalTime utc . addUTCTime x . localTimeToUTC utc


||| diffLocalTime a b = a - b
export diffLocalTime : LocalTime -> LocalTime -> NominalDiffTime
diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)

||| Get the local time of a UT1 time on a particular meridian (in degrees, positive is East).
export ut1ToLocalTime : Rational -> UniversalTime -> LocalTime
ut1ToLocalTime long date = let
  localTime = date.modJulianDate + long / 360
  localMJD  = floor localTime
  localToDOffset = localTime - (fromIntegral localMJD)
  in MkLocalTime (ModifiedJulianDay localMJD) (dayFractionToTimeOfDay localToDOffset)

||| Get the UT1 time of a local time on a particular meridian (in degrees, positive is East).
export localTimeToUT1 : Rational -> LocalTime -> UniversalTime
localTimeToUT1 long (LocalTime (ModifiedJulianDay localMJD) tod) =
    ModJulianDate ((fromIntegral localMJD) + (timeOfDayToDayFraction tod) - (long / 360))

public export
Show UniversalTime where
    show t = show (ut1ToLocalTime 0 t)



