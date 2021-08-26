||| A general purpose Hashing library (not cryptographic)
||| 
|||   The core hash is djb2, which is very fast but does not have the best distribution
|||   Source: http://www.cse.yorku.ca/~oz/hash.html
||| 
|||   The salted version and the magic salt are copied from Haskell's bloomfilter library
|||   Source: https://hackage.haskell.org/package/bloomfilter-2.0.0.0/docs/src/Data-BloomFilter-Hash.html#Hashable
||| 
|||   Portedd from Idris1 by HATTORI, Hiroki 2021.
module Data.Hash

import Data.Vect

%default total

||| A type that can be hashed
public export
interface Hashable a where
  saltedHash64 : a -> Bits64 -> Bits64 -- value to hash, salt, hash


||| Computes a non cryptographic hash
export hash : Hashable a => a -> Bits64
hash x = saltedHash64 x 0x16fc397cf62f64d3


||| Given a user provided salt, computes a non cryptographic hash.
||| This version is meant to mitigate hash-flooding DoS attacks.
export saltedHash : Hashable a => Bits64 -> a -> Bits64
saltedHash salt x = saltedHash64 x salt


||| Nth byte of a Bits64
byte : Int -> Bits64 -> Bits64
byte n w = prim__and_Bits64 (prim__shr_Bits64 w $ fromInteger $ cast (n * 8)) 0xff


mod64 : Integer -> Bits64
mod64 i = fromInteger $ (abs i `mod` 0xffffffffffffffff)


export
Hashable Bits64 where
  saltedHash64 w salt = foldr (\b,acc => (acc `prim__shl_Bits64` 10) + acc + b)
                              salt
                              [byte (fromInteger n) w | n <- [7,6..0]] -- djb2 hash function. Not meant for crypto

export
Hashable a => Hashable (Maybe a) where
  saltedHash64 Nothing  salt = salt
  saltedHash64 (Just k) salt = saltedHash64 k salt

export
(Hashable a, Hashable b) => Hashable (a, b) where
  saltedHash64 (a,b) salt = saltedHash64 b (saltedHash64 a salt)

export
Hashable a => Hashable (List a) where
  saltedHash64 l salt = foldr (\c,acc => saltedHash64 c acc) salt l

export
Hashable a => Hashable (Vect n a) where
  saltedHash64 l salt = foldr (\c,acc => saltedHash64 c acc) salt l

export Hashable Integer where saltedHash64 = saltedHash64 . mod64
export Hashable () where      saltedHash64 _ salt = salt

export
Hashable Bool where
  saltedHash64 True  = saltedHash64 (the Bits64 1)
  saltedHash64 False = saltedHash64 (the Bits64 0)

export Hashable Int where     saltedHash64 = saltedHash64 . the Integer . cast
export Hashable Char where    saltedHash64 = saltedHash64 . the Integer . cast
export Hashable Bits8 where   saltedHash64 = saltedHash64 . prim__cast_Bits8Int
export Hashable Bits16 where  saltedHash64 = saltedHash64 . prim__cast_Bits16Int
export Hashable Bits32 where  saltedHash64 = saltedHash64 . prim__cast_Bits32Int
export Hashable String where  saltedHash64 s = saltedHash64 (unpack s)

