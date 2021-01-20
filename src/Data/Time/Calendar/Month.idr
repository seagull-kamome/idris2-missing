module Data.Time.Calendar.Month

import public Data.Time.Calendar.Types
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian

%default total


||| An absolute count of common calendar months.
||| Number is equal to @(year * 12) + (monthOfYear - 1)@.
export data Month = MkMonth Integer

public export Cast Month Integer where cast (MkMonth x) = x
public export Cast Integer Month where cast = MkMonth

public export Eq Month where (MkMonth x) == (MkMonth y) = x == y
public export Ord Month where compare (MkMonth x) (MkMonth y) = compare x y

export addMonths : Integer -> Month -> Month
addMonths n (MkMonth x) = MkMonth $ x + n

export diffMonths : Month -> Month -> Integer
diffMonths (MkMonth x) (MkMonth y) = x - y

export toYearMonth : Month -> (Year, MonthOfYear)
toYearMonth (MkMonth x) =
  let y = x `div` 12
      m = cast $ x `mod` 12
    in (y, m + 1)

export fromYearMonth : Year -> MonthOfYear -> Month
fromYearMonth y m =
 let m' = if m < 1 then 1 else if m > 12 then 12 else m
  in MkMonth $ y * 12 + cast (m' - 1)

export fromYearMonthValid : Year -> MonthOfYear -> Maybe Month
fromYearMonthValid y m =
  if m < 1 || m > 12 then Nothing
  else Just $ MkMonth $ y * 12 + cast (m - 1)



toMonthDay : Day -> (Month, DayOfMonth)
toMonthDay d = let
  (y, m, d) = toGregorian d
  in (MkMonth (y * 12 + cast (max 0 $ min 11 m)), d)


fromMonth : Month -> DayOfMonth -> Day
fromMonth m dm = let
  (y, my) = toYearMonth m
  in fromGregorian y my dm


fromMonthDayValid : Month -> DayOfMonth -> Maybe Day
fromMonthDayValid m dm = let 
  (y, my) = toYearMonth m
  in fromGregorianValid y my dm

-- vim: tw=80 sw=2 expandtab :
