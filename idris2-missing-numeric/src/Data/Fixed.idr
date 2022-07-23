||| Fixed point fractional number. - Ported from Haskell base.
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Fixed

import Data.Nat
import Data.Maybe
import Data.String
import Data.Rational
import Generics.Newtype

%default total
%language ElabReflection

-- --------------------------------------------------------------------------

public export
record Fixed (n:Nat) where
  constructor MkFixed
  num : Integer
%runElab derive "Fixed" [Generic, Eq, Ord, DecEq]



public export
{n:Nat} -> Num (Fixed n) where
  (MkFixed x) + (MkFixed y) = MkFixed $ x + y
  (MkFixed x) * (MkFixed y) = MkFixed $ (x * y) `div` (cast $ power 10 n)
  fromInteger i = MkFixed $ i * (cast $ power 10 n)

public export
{n:Nat} -> Neg (Fixed n) where
  negate (MkFixed x) = MkFixed $ negate x
  (MkFixed x) - (MkFixed y) = MkFixed $ x - y

public export
{n:Nat} -> Abs (Fixed n) where abs (MkFixed x) = MkFixed $ abs x

partial public export
{n:Nat} -> Fractional (Fixed n) where
  (MkFixed x) / (MkFixed y) = MkFixed $ (x * (cast $ power 10 n)) `div` y
  recip (MkFixed x) = let
    res = cast $ power 10 n in MkFixed $ (res * res) `div` x


public export
{n:Nat} -> Show (Fixed n) where
  show (MkFixed x) = if n == 0 then show x
                               else if x < 0 then "-" ++ go (negate x)
                               else go x where
    go : Integer -> String
    go x = let r = cast $ power 10 n
               i = x `div` r
               d' = show $ x `mod` r
            in show i ++ "." ++ replicate (fromInteger $ cast $ cast n - strLength d') '0' ++ d'



-- --------------------------------------------------------------------------
-- Type cast


public export
{n:Nat} -> Cast Int (Fixed n) where
  cast i = MkFixed $ (cast i) * (cast $ power 10 n)
public export
{n:Nat} -> Cast Integer (Fixed n) where
  cast i = MkFixed $ i * (cast $ power 10 n)
public export
{n:Nat} -> Cast Rational (Maybe (Fixed n)) where
  cast x = let d = x.den
            in toMaybe (d == 0) $ MkFixed $ x.num * (cast $ power 10 n) `div` (natToInteger d)
public export
{n:Nat} -> Cast (Fixed n) Rational where
  cast (MkFixed x) = x %: (cast $ power 10 n)



-- --------------------------------------------------------------------------

public export resolution : {n:Nat} -> Fixed n -> Integer
resolution _ = cast $ power 10 n


export div' : {n:Nat} -> Fixed n -> Fixed n -> Integer
div' {n=n} (MkFixed x) (MkFixed q) = x `div` q

export mod' : {n:Nat} -> Fixed n -> Fixed n -> Fixed n
mod' {n=n} (MkFixed x) (MkFixed q) = MkFixed $ x `mod` q

export scale : {n:Nat} -> Integer -> Fixed n -> Fixed n
scale {n=n} y (MkFixed x) = MkFixed $ x * y

-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
