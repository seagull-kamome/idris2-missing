module Data.Bitset.IOBitset

import Data.So
import Data.IOArray.Prims

%default total

||| Simple flat array bitset.

export
record IOBitset (n:Int) where
  constructor MkIOBitset
  content : ArrayData Bits64


||| Create a new Bitset.
||| @ n   Size of Bitset.
export newIOBitset : HasIO io => (n:Int) -> {auto _:So (n > 0)} -> io (IOBitset n)
newIOBitset n = do
  let k = (n + 63) `div` 64
  pure $ MkIOBitset !(primIO $ prim__newArray k 0)


||| Insert number to Bitset
||| @ m    THe number to insert.
||| @ bs   Bitset
export insert : HasIO io => {n:Int} -> (m:Int) ->
                {auto _:So (n > m)} -> {auto _:So (m >= 0)}  -- NOTE: or should to be Fin?
                -> IOBitset n -> io ()
insert {n=n} m bs = do
  let i = m `div` 64
  let j = m `mod` 64
  x <- primIO (prim__arrayGet bs.content i)
  primIO $ prim__arraySet bs.content i $ prim__or_Bits64 x $ prim__shl_Bits64 1 $ fromInteger $ cast j


||| Remove number from Bitset
||| @ m    The number to remove.
||| @ bs   Bitset
export remove : HasIO io => {n:Int} -> (m:Int) ->
                {auto _:So (n > m)} -> {auto _:So (m >= 0)}
                -> IOBitset n -> io ()
remove {n=n} m bs = do
  let i = m `div` 64
  let j = m `mod` 64
  let msk = prim__or_Bits64 (prim__shr_Bits64 0xffffffffffffffff $ fromInteger $ cast (64 - j))
                            (prim__shl_Bits64 0xffffffffffffffff $ fromInteger $ cast (j + 1))
  x <- primIO (prim__arrayGet bs.content i)
  primIO $ prim__arraySet bs.content i $ prim__and_Bits64 x msk


||| 
||| @ m   The number to test
||| @ bs  Bitset
export lookup : HasIO io => {n:Int} -> (m:Int) ->
                {auto _:So (n > m)} -> {auto _:So (m >= 0)}
                -> IOBitset n -> io Bool
lookup {n=n} m bs = do
  let i = m `div` 64
  let j = m `mod` 64
  x <- primIO (prim__arrayGet bs.content i)
  pure $ 0 /= (prim__and_Bits64 x $ prim__shl_Bits64 1 $ fromInteger $ cast j)


