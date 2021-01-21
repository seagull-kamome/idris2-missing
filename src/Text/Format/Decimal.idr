||| Format decimal number into string.
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Text.Format.Decimal

import Data.Maybe

import Data.Strings
import Data.String.Extra
import Data.List

import Data.Fixed

%default total

-- ---------------------------------------------------------------------------

public export
record DecimalFormat where
  constructor MkDecimalFormat
  width : Maybe Nat
  prec  : Maybe Nat
  pad   : Maybe Char
  spc   : Char
  point : Char
  minus : Maybe (Either String String)
  plus  : Maybe (Either String String)

export defaultDecimalFormat : DecimalFormat
defaultDecimalFormat =
  MkDecimalFormat Nothing Nothing Nothing ' ' '.' (Just $ Left "-") Nothing

export zeroPad : Nat -> DecimalFormat -> DecimalFormat
zeroPad w = record { width = Just w, pad = Just '0' }



-- ---------------------------------------------------------------------------

lpad : Nat -> Char -> String -> String
lpad n c str = replicate (fromInteger $ cast $ cast n - strLength str) c ++ str

formatDecimal : DecimalFormat -> Maybe (Either String String) -> String -> String -> String
formatDecimal fmt sign dec frc = let
  dec' = case (fmt.width, fmt.pad) of
              (Just w, Just c) => lpad w c dec
              _ => dec
  frc' = case fmt.prec of
              Nothing => if frc == "" then "" else (singleton fmt.point ++ frc)
              Just 0  => ""
              Just n  => (singleton fmt.point) ++ frc ++ lpad n '0' frc
  str  = case sign of
              Nothing => lpad (fromMaybe 0 fmt.width) ' ' dec' ++ frc'
              Just (Left s) => lpad (fromMaybe 0 fmt.width) ' ' (s ++ dec') ++ frc'
              Just (Right s) => lpad (fromMaybe 0 fmt.width) ' ' dec' ++ frc' ++ s
  in case fmt.width of
          Nothing => str
          Just n  => replicate (fromInteger $ cast $ cast n - strLength str) fmt.spc
                     ++ str


public export formatIntegral : (Show ty, Neg ty, Num ty, Ord ty) => DecimalFormat -> ty -> String
formatIntegral fmt x =
  if x < 0
     then formatDecimal fmt fmt.minus (show $ negate x) ""
     else formatDecimal fmt fmt.plus (show x) ""

public export formatFixed : {n:Nat} -> DecimalFormat -> Fixed n -> String
formatFixed {n=n} fmt (MkFixed x') with (n)
  formatFixed {n=n} fmt (MkFixed x') | 0 = formatIntegral fmt x'
  formatFixed {n=n} fmt (MkFixed x') | _ = let
    (x, sign) = if x' < 0 then (negate x', fmt.minus) else (x', fmt.plus)
    r = expToResolution n
    dec = show $ x `div` r
    frc' = show $ x `mod` r
    frc = replicate (fromInteger $ cast $ cast n - strLength frc') '0' ++ frc'
    in formatDecimal fmt sign dec frc


public export interface FormatDecimal t where format : DecimalFormat -> t -> String
public export FormatDecimal Int where format = formatIntegral
public export FormatDecimal Integer where format = formatIntegral
public export {n:Nat} -> FormatDecimal (Fixed n) where format = formatFixed {n=n}

public export format' : FormatDecimal ty => (DecimalFormat -> DecimalFormat) -> ty -> String
format' f x = format (f defaultDecimalFormat) x

-- vim: tw=80 sw=2 expandtab :
