module Data.Time.Calendar.Days

%default total

-- ---------------------------------------------------------------------------

public export
record Day where
  constructor ModifiedJulianDay
  modifiedJulianDay : Integer

public export Eq Day where x == y = x.modifiedJulianDay == y.modifiedJulianDay
public export Ord Day where compare x y = compare x.modifiedJulianDay y.modifiedJulianDay

export addDays : Integer -> Day -> Day
addDays n x = { modifiedJulianDay $= (+n) } x

export diffDays : Day -> Day -> Integer
diffDays x y = x.modifiedJulianDay - y.modifiedJulianDay


