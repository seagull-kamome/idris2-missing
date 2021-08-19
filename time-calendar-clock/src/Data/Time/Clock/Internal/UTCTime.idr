module Data.Time.Clock.Internal.UTCTime

import Data.Time.Clock.DiffTime
import Data.Time.Calendar.Days

%default total


public export
record UTCTime where
  constructor MkUTCTime
  day : Day
  daytime : DiffTime

public export Eq UTCTime where x == y = x.day == y.day && x.daytime == y.daytime
public export
Ord UTCTime where
  compare x y = let c = compare x.day y.day
                 in case c of
                   EQ => compare x.daytime y.daytime
                   c' => c'


