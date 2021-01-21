||| Format decimal number into string.
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Text.Format.Decimal

import Data.Strings
import Data.String.Extra
import Data.List

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

public export formatIntegral : (Show ty, Neg ty, Num ty, Ord ty) => DecimalFormat -> ty -> String
formatIntegral fmt x' = let
  (x, sign) = if x' < 0 then (negate x', fmt.minus) else (x', fmt.plus)
  dec' = let x' = show x
          in case (fmt.width, fmt.pad) of
                  (Just w, Just c) => replicate (fromInteger $ cast $ cast w - strLength x') c ++ x'
                  _ => x'
  frc  = case fmt.prec of
              Nothing => ""
              Just 0  => ""
              Just n  => (singleton fmt.point) ++ (concat (replicate n "0"))
  str  = case sign of
              Nothing => dec' ++ frc
              Just (Left s) => s ++ dec' ++ frc
              Just (Right s) => dec' ++ frc ++ s
  in case fmt.width of
          Nothing => str
          Just n  => replicate (fromInteger $ cast $ cast n - strLength str) fmt.spc
                     ++ str


public export interface FormatDecimal t where format : DecimalFormat -> t -> String
public export FormatDecimal Int where format = formatIntegral
public export FormatDecimal Integer where format = formatIntegral

public export format' : FormatDecimal ty => (DecimalFormat -> DecimalFormat) -> ty -> String
format' f x = format (f defaultDecimalFormat) x

-- vim: tw=80 sw=2 expandtab :
