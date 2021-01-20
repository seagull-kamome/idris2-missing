module Data.Time.Clock.UTCTime

import Data.Fixed
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

-- ---------------------------------------------------------------------------

export addUTCTime : NominalDiffTime -> UTCTime -> UTCTime
addUTCTime a b =
  let s = a.seconds + b.daytime.seconds
      d = s `div'` nominalDay.seconds
      t = s `mod'` nominalDay.seconds
      in MkUTCTime (addDays d b.day) (secondsToDiffTime t)


export diffUTCTime : UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime a b =
  let d = diffDays a.day b.day
      t = a.daytime.seconds - b.daytime.seconds
      in SecondsToDiffTime $ scale d nominalDay.seconds + t


