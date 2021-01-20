module Data.Time.Clock

import Data.Fixed
import Data.Rational

import public Data.Time.Clock.DiffTime
import public Data.Time.Clock.UTCTime
import Data.Time.Calendar

%default total

-- ---------------------------------------------------------------------------

public export
record UniversalTime where
  constructor ModJulianDate
  modJulianDate : Rational


public export
Eq UniversalTime where
  x == y = x.modJulianDate == y.modJulianDate

public export
Ord UniversalTime where
  compare x y = compare x.modJulianDate y.modJulianDate

-- Show
-- FormatTime

-- ---------------------------------------------------------------------------




