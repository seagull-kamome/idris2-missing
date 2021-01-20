module Data.Bitset.Tiny

import Data.Bitset.Classes

%default total

-- ---------------------------------------------------------------------------

public export
record TinyBitset t where
  constructor MkTBS
  bits : t


export ValidTinyBitsets : List Type
ValidTinyBitsets = [ Bits8, Bits16, Bits32, Bits64 ]


export Semigroup (TinyBitset Bits8) where MkTBS l <+> MkTBS r = MkTBS $ prim__or_Bits8 l r
export Semigroup (TinyBitset Bits16) where MkTBS l <+> MkTBS r = MkTBS $ prim__or_Bits16 l r
export Semigroup (TinyBitset Bits32) where MkTBS l <+> MkTBS r = MkTBS $ prim__or_Bits32 l r
export Semigroup (TinyBitset Bits64) where MkTBS l <+> MkTBS r = MkTBS $ prim__or_Bits64 l r

export Monoid (TinyBitset Bits8) where  neutral = MkTBS 0
export Monoid (TinyBitset Bits16) where neutral = MkTBS 0
export Monoid (TinyBitset Bits32) where neutral = MkTBS 0
export Monoid (TinyBitset Bits64) where neutral = MkTBS 0

-- ---------------------------------------------------------------------------


export
Set Int (TinyBitset Bits8) where
  insert n x@(MkTBS bs) =
    if n >= 0 && n < 8
       then MkTBS $ prim__or_Bits8 bs $ prim__shl_Bits8 1 $ fromInteger $ cast n
       else x
  remove n x@(MkTBS bs) =
    if n >= 0 && n < 8
       then MkTBS $ prim__and_Bits8 bs $ prim__xor_Bits8 0xff $ prim__shl_Bits8 1 $ fromInteger $ cast n
       else x
  lookup n (MkTBS bs) = 0 /= (prim__and_Bits8 bs $ prim__shl_Bits8 1 $ fromInteger $ cast n)

export
Set Int (TinyBitset Bits16) where
  insert n x@(MkTBS bs) =
    if n >= 0 && n < 16
       then MkTBS $ prim__or_Bits16 bs $ prim__shl_Bits16 1 $ fromInteger $ cast n
       else x
  remove n x@(MkTBS bs) =
    if n >= 0 && n < 16
       then MkTBS $ prim__and_Bits16 bs $ prim__xor_Bits16 0xffff $ prim__shl_Bits16 1 $ fromInteger $ cast n
       else x
  lookup n (MkTBS bs) = 0 /= (prim__and_Bits16 bs $ prim__shl_Bits16 1 $ fromInteger $ cast n)

export
Set Int (TinyBitset Bits32) where
  insert n x@(MkTBS bs) =
    if n >= 0 && n < 32
       then MkTBS $ prim__or_Bits32 bs $ prim__shl_Bits32 1 $ fromInteger $ cast n
       else x
  remove n x@(MkTBS bs) =
    if n >= 0 && n < 32
       then MkTBS $ prim__and_Bits32 bs $ prim__xor_Bits32 0xffffffff $ prim__shl_Bits32 1 $ fromInteger $ cast n
       else x
  lookup n (MkTBS bs) = 0 /= (prim__and_Bits32 bs $ prim__shl_Bits32 1 $ fromInteger $ cast n)

export
Set Int (TinyBitset Bits64) where
  insert n x@(MkTBS bs) =
    if n >= 0 && n < 64
       then MkTBS $ prim__or_Bits64 bs $ prim__shl_Bits64 1 $ fromInteger $ cast n
       else x
  remove n x@(MkTBS bs) =
    if n >= 0 && n < 64
       then MkTBS $ prim__and_Bits64 bs $
                      prim__or_Bits64 (prim__shr_Bits64 0xffffffffffffffff $ fromInteger $ cast (64 - n))
                                      (prim__shl_Bits64 0xffffffffffffffff $ fromInteger $ cast (n + 1))
       else x
  lookup n (MkTBS bs) = 0 /= (prim__and_Bits64 bs $ prim__and_Bits64 1 $ fromInteger $ cast n)



