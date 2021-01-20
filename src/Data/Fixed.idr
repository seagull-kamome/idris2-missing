module Data.Fixed

import Data.List
import Data.Strings

%default total

-- --------------------------------------------------------------------------

public export
data Fixed : Nat -> Type where
  MkFixed : Integer -> Fixed n

expToResolution : Nat -> Integer
expToResolution = go 1 where
  go : Integer -> Nat -> Integer
  go ans Z = ans
  go ans (S n) = go (ans * 10) n

public export resolution : {n:Nat} -> Fixed n -> Integer
resolution {n=n} _ = expToResolution n


public export Eq (Fixed n) where MkFixed x == MkFixed y = x == y
public export Ord (Fixed n) where compare (MkFixed x) (MkFixed y) = compare x y

public export
{n:Nat} -> Num (Fixed n) where
  (MkFixed x) + (MkFixed y) = MkFixed $ x + y
  (MkFixed x) * (MkFixed y) = MkFixed $ (x * y) `div` (expToResolution n)
  fromInteger i = MkFixed $ i * expToResolution n

public export
{n:Nat} -> Neg (Fixed n) where
  negate (MkFixed x) = MkFixed $ negate x
  (MkFixed x) - (MkFixed y) = MkFixed $ x - y

public export
{n:Nat} -> Abs (Fixed n) where abs (MkFixed x) = MkFixed $ abs x

partial public export
{n:Nat} -> Fractional (Fixed n) where
  (MkFixed x) / (MkFixed y) = MkFixed $ (x * expToResolution n) `div` y
  recip (MkFixed x) = let r = expToResolution n in MkFixed $ (r * r) `div` x


public export
{n:Nat} -> Show (Fixed n) where
  show (MkFixed x) = if n == 0 then show x
                               else if x < 0 then "-" ++ go x
                               else go x where
    go : Integer -> String
    go x = let r = expToResolution n
               i = x `div` r
               d' = show $ x `mod` r
            in show i ++ "." ++ concat (replicate (fromInteger (r - cast (strLength d'))) " ") ++ d'

-- --------------------------------------------------------------------------

export div' : {n:Nat} -> Fixed n -> Fixed n -> Integer
div' {n=n} (MkFixed x) (MkFixed q) = x `div` q

export mod' : {n:Nat} -> Fixed n -> Fixed n -> Fixed n
mod' {n=n} (MkFixed x) (MkFixed q) = MkFixed $ x `mod` q

export scale : {n:Nat} -> Integer -> Fixed n -> Fixed n
scale {n=n} y (MkFixed x) = MkFixed $ x * y

