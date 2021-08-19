||| Fixed point fractional number. - Ported from Haskell base.
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Fixed

import Data.Maybe
import Data.Strings

import Data.Rational

%default total

-- --------------------------------------------------------------------------

namespace ResolutionV

  public export
  data ResolutionV : Nat -> Type where
    MkResV : (n:Nat) -> (res:Integer) -> (res > 0 = True) -> ResolutionV n

  export expToResolution : (n:Nat) -> ResolutionV n
  expToResolution n = go 1 Refl n where
    go : (ans:Integer) -> (ans > 0 = True) -> (m:Nat) -> ResolutionV n
    go ans prf Z = MkResV n ans prf
    go ans prf (S m) = go (ans * 10) (believe_me $ Refl {x=True}) m




-- --------------------------------------------------------------------------

public export
data Fixed : Nat -> Type where
  MkFixed : Integer -> Fixed n

public export Eq (Fixed n) where MkFixed x == MkFixed y = x == y
public export Ord (Fixed n) where compare (MkFixed x) (MkFixed y) = compare x y

public export
{n:Nat} -> Num (Fixed n) where
  (MkFixed x) + (MkFixed y) = MkFixed $ x + y
  (MkFixed x) * (MkFixed y) = let
    MkResV _ res _ = expToResolution n in MkFixed $ (x * y) `div` res
  fromInteger i = let
    MkResV _ res _ = expToResolution n in MkFixed $ i * res

public export
{n:Nat} -> Neg (Fixed n) where
  negate (MkFixed x) = MkFixed $ negate x
  (MkFixed x) - (MkFixed y) = MkFixed $ x - y

public export
{n:Nat} -> Abs (Fixed n) where abs (MkFixed x) = MkFixed $ abs x

partial public export
{n:Nat} -> Fractional (Fixed n) where
  (MkFixed x) / (MkFixed y) = let
    MkResV _ res _ = expToResolution n in MkFixed $ (x * res) `div` y
  recip (MkFixed x) = let
    MkResV _ res _ = expToResolution n in MkFixed $ (res * res) `div` x


public export
{n:Nat} -> Show (Fixed n) where
  show (MkFixed x) = if n == 0 then show x
                               else if x < 0 then "-" ++ go (negate x)
                               else go x where
    go : Integer -> String
    go x = let MkResV _ r _ = expToResolution n
               i = x `div` r
               d' = show $ x `mod` r
            in show i ++ "." ++ replicate (fromInteger $ cast $ cast n - strLength d') '0' ++ d'

-- --------------------------------------------------------------------------
-- Type cast


public export
{n:Nat} -> Cast Int (Fixed n) where
  cast i = let MkResV _ res _ = expToResolution n in MkFixed $ (cast i) * res
public export
{n:Nat} -> Cast Integer (Fixed n) where
  cast i = let MkResV _ res _ = expToResolution n in MkFixed $ i * res
public export
{n:Nat} -> Cast Rational (Maybe (Fixed n)) where
  cast x = let d = x.den
               MkResV _ res _ = expToResolution n
            in toMaybe (d == 0) $ MkFixed $ x.num * res `div` (natToInteger d)
public export
{n:Nat} -> Cast (Fixed n) Rational where
  cast (MkFixed x) = let
    MkResV _ res _ = expToResolution n in x %: res

-- --------------------------------------------------------------------------

public export resolution : {n:Nat} -> Fixed n -> Integer
resolution {n=n} _ = let MkResV _ res _ = expToResolution n in res


export div' : {n:Nat} -> Fixed n -> Fixed n -> Integer
div' {n=n} (MkFixed x) (MkFixed q) = x `div` q

export mod' : {n:Nat} -> Fixed n -> Fixed n -> Fixed n
mod' {n=n} (MkFixed x) (MkFixed q) = MkFixed $ x `mod` q

export scale : {n:Nat} -> Integer -> Fixed n -> Fixed n
scale {n=n} y (MkFixed x) = MkFixed $ x * y

-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
