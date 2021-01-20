module Data.Time.Clock.DiffTime

import Data.Fixed

%default total


public export
record DiffTime' (nominal:Bool) where
  constructor SecondsToDiffTime
  seconds : Fixed 12

public export DiffTime : Type
DiffTime = DiffTime' False

public export NominalDiffTime : Type
NominalDiffTime = DiffTime' True


public export Eq (DiffTime' b) where x == y = x.seconds == y.seconds
public export Ord (DiffTime' b) where compare x y = compare x.seconds y.seconds
public export
Num (DiffTime' b) where
  x + y = SecondsToDiffTime $ x.seconds + y.seconds
  x * y = SecondsToDiffTime $ x.seconds * y.seconds
  fromInteger x = SecondsToDiffTime $ fromInteger x
public export
Neg (DiffTime' b) where
  negate x = { seconds $= negate } x
  x - y = SecondsToDiffTime $ x.seconds - y.seconds



export %inline picosecondsToDiffTime : {nominal:Bool} -> Integer -> DiffTime' nominal
picosecondsToDiffTime x = SecondsToDiffTime $ MkFixed x

export %inline secondsToDiffTime : {nominal:Bool} -> Fixed 12 -> DiffTime' nominal
secondsToDiffTime x = SecondsToDiffTime x


export nominalDay : NominalDiffTime
nominalDay = 86400



