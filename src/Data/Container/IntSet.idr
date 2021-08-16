||| Using single machine-word as Bitarray.
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Container.IntSet

import Data.Container.Interfaces

import Data.Bits
import Data.Maybe               -- fromMaybe
import Data.So
import Data.SortedMap as M      -- mergeWith, empty
import Decidable.Equality

%default total

-- --------------------------------------------------------------------------

-- 0 decideTiny : (n:Nat) -> Dec ((n <= 32) = True)
decideTiny : (n:Nat) -> Dec ((n <= 32) = True)
decideTiny n = decEq (n <= 32) True

export IntSet : (N:Nat) -> Type
IntSet n with (decideTiny n)
  IntSet _ | Yes _ = Bits32
  IntSet _ | No _  = M.SortedMap Integer Bits32

-- --------------------------------------------------------------------------

export
(n:Nat) => Semigroup (IntSet n) where
  x <+> y with (decideTiny n)
    x <+> y | Yes _ = prim__or_Bits32 x y
    x <+> y | No _  = M.mergeWith prim__or_Bits32 x y

export
(n:Nat) => Semigroup (IntSet n) => Monoid (IntSet n) where
  neutral with (decideTiny n)
    neutral | Yes _ = 0
    neutral | No _  = M.empty

-- --------------------------------------------------------------------------

keyix : Integer -> (Integer, Bits32)
keyix k =
  let i = if k >= 0 then (k `div` 32) else (negate $ (k - 1) `div` 32)
      b = fromInteger $ abs $ k `mod` 32
   in (i, b)


export
{n:Nat} -> Container (IntSet n) where
  KeyTy = Integer
  null xs with (decideTiny n)
    null xs | Yes _ = xs == 0
    null xs | No _  = null xs
  count xs with (decideTiny n)
    count xs | Yes _ = popCount xs
    count xs | No _  = foldl (\acc, b => acc + popCount b) 0 xs
  contains k xs with (decideTiny n)
    contains k xs | Yes _ =
      if k >= 0 && k < 32
        then 0 /= (prim__and_Bits32 xs $ prim__shl_Bits32 1 $ fromInteger k)
        else False
    contains k xs | No  _ =
      let (i, b) = keyix k
       in maybe False (\ys => 0 /= (prim__and_Bits32 ys $ prim__shl_Bits32 1 b))
                (M.lookup i xs)
  delete k xs with (decideTiny n)
    delete k xs | Yes _ =
      if k >= 0 && k < 32
         then prim__and_Bits32 xs $ prim__xor_Bits32 0xffffffff $ prim__shl_Bits32 1 $ fromInteger k
         else xs
    delete k xs | No _  =
      let (i, b) = keyix k
       in maybe xs (\ys => M.insert i (prim__and_Bits32 ys $ prim__xor_Bits32 0xffffffff $ prim__shl_Bits32 1 b) xs)
                (M.lookup i xs)

export
(n:Nat) => (Container (IntSet n)) => Set (IntSet n) where
  KeyTy = Integer
  insert k xs with (decideTiny n)
    insert k xs | Yes _ =
      if k >= 0 && k < 32
         then prim__or_Bits32 xs $ prim__shl_Bits32 1 $ fromInteger k
         else xs
    insert k xs | No  _ =
      let (i, b) = keyix k
       in M.insert i
                   (prim__or_Bits32 (fromMaybe 0 (M.lookup i xs)) $ prim__shl_Bits32 1 b)
                   xs




-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
