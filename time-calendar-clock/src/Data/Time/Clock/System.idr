||| System clock time.
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Data.Time.Clock.System

import System.Info

import Data.Fixed

import Data.Time.Calendar.Types
import Data.Time.Calendar.Days
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.AbsoluteTime
import Data.Time.Clock.DiffTime
import Data.Time.LocalTime.TimeOfDay
import Data.Time.LocalTime.TimeZone

%default total

-- ---------------------------------------------------------------------------

data PrimSystemTime where [external
data ReplSystemTime = MkSystemTime (seconds:Integer) (nanoseconds:Int)

%inline public export SystemTime : Type
SystemTimee with (codegen)
  systemTimeType | "chez"        = PrimSystemTime
  systemTimeType | "javascript"  = ReplSystemTime
  systemTimeType | "node"        = ReplSystemTime
  systemTimeType | "refc"        = ReplSystemTime
  systemTimeType | _             = ReplSystemTime



-- ---------------------------------------------------------------------------

%foreign "scheme,chez:time-second"
         "javascript:lambda:(x) => (x.getTime() / 1000)"
         "C:idris2_crash"
prim__systemSeconds : PrimSystemTime -> Int

%foreign "scheme,chez:time-nanosecond"
         "javascript:lambda:(x) => (x.getTime() * 1000000)"
         "C:idris2_crash"
prim__systemNanoseconds : PrimSystemTime -> Int

%foreign "scheme,chez:(lambda (s ns) (make-time 'time-utc ns s))"
         "javascript:lambda:(s, ns) => new Date(s * 1000 + (ns / 1000000))"
         "C:idris2_crash"
prim__toSystemTime : Int -> Int -> PrimSystemTime

%foreign "scheme,chez:current-time"
         "javascript:lambda:() => new Date()"
prim__getSystemTime : PrimIO PrimSystemTime


-- ---------------------------------------------------------------------------

export systemSeconds : SystemTime -> Integer
systemSeconds ts with (SystemTime)
  systemSeconds x                   | PrimSystemTime = cast $ prim__systemSeconds x
  systemSeconds (MkSystemTime s ns) | RepresentedSystemTime = s

export systemNanoseconds : SystemTime -> Int
systemNanoseconds ts with (SystemTimeType)
  systemNanoseconds x                   | PrimSystemTime = prim__systemNanoseconds x
  systemNanoseconds (MkSystemTime s ns) | RepresentedSystemTime = ns


export toSystemTime : Integer -> Int -> SystemTime
toSystemTime s ns = go where
   s' = s + cast (ns `div` 1000000000)
   ns' = ns `mod` 1000000000
   go | with (systemTimeType)
     go | PrimSystemTime = prim__toSystemTime (cast s') ns'
     go | ReplSystemTime = MkSystemTime s ns

export getSystemTime : HasIO io => io SystemTime
getSystemTime with (systemTimeType)
  getSystemTime | ReplSystemTime = pure $ MkSystemTime 0 0 -- FIXME:
  getSystemTime | PrimSystemTime = primIO prim__getSystemTime


public export
Show SystemTime where
  show x = "#" ++ show (systemSeconds x) ++ "s+"
           ++ show (systemNanoseconds x) ++ "ns"


||| Map leap-second values to the start of the following second.
||| The resulting 'systemNanoseconds' will always be in the range 0 to 1E9-1.
export truncateSystemTimeLeapSecond : SystemTime -> SystemTime
truncateSystemTimeLeapSecond t =
  if systemNanoseconds t >= 1000000000
     then toSystemTime (systemSeconds t + 1) 0
     else t


||| The day of the epoch of 'SystemTime', 1970-01-01
export systemEpochDay : Day
systemEpochDay = ModifiedJulianDay 40587

export systemEpochAbsolute : AbsoluteTime
systemEpochAbsolute = taiNominalDayStart systemEpochDay





||| Convert 'SystemTime' to 'UTCTime', matching zero 'SystemTime' to midnight of 'systemEpochDay' UTC.
export systemToUTCTime : SystemTime -> UTCTime
systemToUTCTime t = let
  days = systemSeconds t `div` 86400
  day = addDays days systemEpochDay
  --
  timeseconds = systemSeconds t `mod` 86400
  timeNanoseconds = timeseconds * 1000000000 + cast (systemNanoseconds t)
  timePicoseconds = timeNanoseconds * 1000
  in MkUTCTime day $ picosecondsToDiffTime timePicoseconds


export getCurrentTime : HasIO io => io UTCTime
getCurrentTime = [| systemToUTCTime getSystemTime |]


||| Convert 'UTCTime' to 'SystemTime', matching zero 'SystemTime' to midnight of 'systemEpochDay' UTC.
export utcToSystemTime : UTCTime -> SystemTime
utcToSystemTime (MkUTCTime day time) = let
    days = diffDays day systemEpochDay
    timeNanoseconds = diffTimeToPicoseconds time `div` 1000
    (timeSeconds, nanoseconds) =
        if timeNanoseconds >= 86400000000000
            then (86399, timeNanoseconds - 86399000000000)
            else (timeNanoseconds `div` 1000000000, timeNanoseconds `mod` 1000000000)
    seconds = days * 86400 + timeSeconds
    in toSystemTime seconds $ cast nanoseconds


||| Convert 'SystemTime' to 'AbsoluteTime', matching zero 'SystemTime' to midnight of 'systemEpochDay' TAI.
export systemToTAITime : SystemTime -> AbsoluteTime
systemToTAITime t = let
    s = systemSeconds t
    ns = systemNanoseconds t
    diff = secondsToDiffTime (fromInteger s) + picosecondsToDiffTime (cast ns * 1000)
    in addAbsoluteTime diff systemEpochAbsolute





-- ---------------------------------------------------------------------------

data Prim__SystemLocalTime : Type where [external]

public export
data SystemLocalTime' : SystemTimeType -> Type where
  MkPrimSystemLocalTime : Prim__SystemLocalTime -> SystemLocalTime' PrimSystemTime

export SystemLocalTime : Type
SystemLocalTime = SystemLocalTime' PrimSystemTime



-- ---------------------------------------------------------------------------

%foreign "scheme,chez:current-date"
         "javascript:lambda:() => new Date()"
export prim__getSystemLocalTime : PrimIO Prim__SystemLocalTime

%foreign "scheme,chez:date-year"
         "javascript:lambda:(x) => x.getFullYear()"
export prim__systemLocalTimeYear : Prim__SystemLocalTime -> Int

%foreign "scheme,chez:date-month"
         "javascript:lambda:(x) => x.getMonth()"
export prim__systemLocalTimeMonth : Prim__SystemLocalTime -> Int

%foreign "scheme,chez:date-day"
         "javascript:lambda:(x) => x.getDate()"
export prim__systemLocalTimeDay : Prim__SystemLocalTime -> Int

%foreign "scheme,chez:date-hour"
         "javascript:lambda:(x) => x.getHours()"
export prim__systemLocalTimeHour : Prim__SystemLocalTime -> Int

%foreign "scheme,chez:date-minute"
         "javascript:lambda:(x) => x.getMinutes()"
export prim__systemLocalTimeMinute : Prim__SystemLocalTime -> Int

%foreign "scheme,chez:date-second"
         "javascript:lambda:(x) => x.getSeconds()"
export prim__systemLocalTimeSecond : Prim__SystemLocalTime -> Int

%foreign "scheme,chez:date-nanosecond"
         "javascript:lambda:(x) => (x.getMilliseconds() * 1000000)"
export prim__systemLocalTimeNanosecond : Prim__SystemLocalTime -> Int

%foreign "scheme,chez:date-zone-offset"
         "javascript:lambda:(x) => x.getTimezoneOffset()"
export prim__systemLocalTimeOffset : Prim__SystemLocalTime -> Int

%foreign "scheme,chez:(lambda (x) \"\")"
         "javascript:lambda:(x) => ''"
export prim__systemLocalTimeZoneName : Prim__SystemLocalTime -> String

%foreign "scheme,chez:(lambda (x) #f)"
         "javascript:lambda:(x) => false"
export prim__systemLocalTimeIsSummerTime : Prim__SystemLocalTime -> Bool

-- ---------------------------------------------------------------------------

export getSystemLocalTime : HasIO io => io SystemLocalTime
getSystemLocalTime = pure $ MkPrimSystemLocalTime !(primIO prim__getSystemLocalTime)


export systemLocalDate : SystemLocalTime -> (Year, MonthOfYear, DayOfMonth)
systemLocalDate (MkPrimSystemLocalTime t) = let
  y = prim__systemLocalTimeYear t
  m = prim__systemLocalTimeMonth t
  d = prim__systemLocalTimeDay t
  in (cast y, m, d)

export systemLocalTimeOfDay : SystemLocalTime -> TimeOfDay
systemLocalTimeOfDay (MkPrimSystemLocalTime t) = let
  h = prim__systemLocalTimeHour t
  m = prim__systemLocalTimeMinute t
  s = prim__systemLocalTimeSecond t
  ns = prim__systemLocalTimeNanosecond t
  in MkTimeOfDay h m (cast s + MkFixed (cast $ ns * 1000))

export systemLocalTimeZone : SystemLocalTime -> TimeZone
systemLocalTimeZone (MkPrimSystemLocalTime t) = let
  m = negate $ prim__systemLocalTimeOffset t `div` 60
  in MkTimeZone m (prim__systemLocalTimeIsSummerTime t) (prim__systemLocalTimeZoneName t)

export getCurrentTimeZone : IO TimeZone
getCurrentTimeZone = getSystemLocalTime >>= pure . systemLocalTimeZone


public export
Show SystemLocalTime where
  show x = let
    (y, m, d) = systemLocalDate x
    td = systemLocalTimeOfDay x
    tz = systemLocalTimeZone x
    in show y ++ "-" ++ show m ++ "-" ++ show d ++ " "
       ++ show td ++ " " ++ show tz


-- vim: tw=80 sw=2 expandtab :
