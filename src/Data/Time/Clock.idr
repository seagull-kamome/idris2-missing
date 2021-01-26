||| Universal clock - Ported from Haskell time package.
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
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

-- ---------------------------------------------------------------------------

-- vim: tw=80 sw=2 expandtab :
