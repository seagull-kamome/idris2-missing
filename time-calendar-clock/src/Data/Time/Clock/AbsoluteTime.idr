module Data.Time.Clock.AbsoluteTime

import Data.Fixed
import Data.Time.Calendar.Days
import Data.Time.Clock.DiffTime

%default total

export data AbsoluteTime = MkAbsoluteTime DiffTime

public export
Eq AbsoluteTime where
  (MkAbsoluteTime x) == (MkAbsoluteTime y) = x == y

public export
Ord AbsoluteTime where
  compare (MkAbsoluteTime x) (MkAbsoluteTime y) = compare x y


||| The epoch of TAI, which is 1858-11-17 00:00:00 TAI.
export taiEpoch : AbsoluteTime
taiEpoch = MkAbsoluteTime 0


export taiNominalDayStart : Day -> AbsoluteTime
taiNominalDayStart day =
  MkAbsoluteTime $ SecondsToDiffTime $ cast $ day.modifiedJulianDay * 86400

||| addAbsoluteTime a b = a + b
export addAbsoluteTime : DiffTime -> AbsoluteTime -> AbsoluteTime
addAbsoluteTime t (MkAbsoluteTime a) = MkAbsoluteTime (a + t)

||| diffAbsoluteTime a b = a - b
export diffAbsoluteTime : AbsoluteTime -> AbsoluteTime -> DiffTime
diffAbsoluteTime (MkAbsoluteTime a) (MkAbsoluteTime b) = a - b



