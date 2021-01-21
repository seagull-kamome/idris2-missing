||| Simple mutable hashtable.
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Data.Hashtable

import Data.So
import Data.List
import Data.IOArray.Prims

import Data.Hash

%default total

export
record IOHashtable k v where
  constructor MkIOHashtable
  maxsize : Int
  content : ArrayData $ List (k, v)

export newHashtable : HasIO io => (n:Int) -> {auto _:So (n > 0)} -> io (IOHashtable k v)
newHashtable n = pure $ MkIOHashtable n !(primIO $ prim__newArray n [])

export insert' : (Hashable k, Eq k, HasIO io) => k -> v -> IOHashtable k v -> io (k, v)
insert' k v tbl = do
  let ix = the Int $ cast $ prim__cast_Bits64Integer (saltedHash 0xdeadbeef k) `mod` (cast tbl.maxsize)
  xs <- primIO (prim__arrayGet tbl.content ix)
  case (find ((k ==) . fst) xs) of
       Just x => pure x
       Nothing => do
         let kv = (k, v) 
         primIO $ prim__arraySet tbl.content ix (kv::xs)
         pure kv

export insert : (Hashable k, Eq k, HasIO io) => k -> v -> IOHashtable k v -> io ()
insert k v tbl = insert' k v tbl >>= \_ => pure ()

export lookup' : (Hashable k, Eq k, HasIO io) => k -> IOHashtable k v -> io (Maybe (k, v))
lookup' k tbl = do
  let ix = cast $ prim__cast_Bits64Integer (saltedHash 0xdeadbeef k) `mod` (cast tbl.maxsize)
  primIO (prim__arrayGet tbl.content ix) >>= pure . find ((k ==) . fst)

export lookup : (Hashable k, Eq k, HasIO io) => k -> IOHashtable k v -> io (Maybe v)
lookup k tbl = lookup' k tbl >>= pure . map snd



