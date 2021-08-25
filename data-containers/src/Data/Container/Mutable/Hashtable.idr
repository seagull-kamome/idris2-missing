||| Simple mutable hashtable.
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Data.Container.Mutable.Hashtable

import Data.Container.Mutable.Array
import Data.Hash

import Data.Maybe
import Data.Fin
import Data.So
import Data.List
import Data.IORef


%default total

-- --------------------------------------------------------------------------

export
record IOHashtable k v where
  constructor MkIOHashtable
  count : IORef Nat
  content : IOArray (List (k, IORef v))


export newHashtable : HasIO io
    => (capacity:Nat)
    -> {auto 0 capacity_isnt_zero: So (capacity /= 0)}
    -> io (IOHashtable k v)
newHashtable capacity@(S _)
  = pure $ MkIOHashtable !(newIORef 0) !(newIOArray capacity [])



export null : HasIO io => IOHashtable k v -> io Bool
null tbl = pure $ !(readIORef tbl.count) == 0

export count : HasIO io => IOHashtable k v -> io Nat
count tbl = readIORef tbl.count




export insert : (Hashable k, Eq k, HasIO io) => k -> v -> IOHashtable k v -> io ()
insert k v tbl = do
  let ix = restrict (ubound tbl.content) (cast $ saltedHash 0xdeadbeef k)
  xs <- readIOArray tbl.content ix
  case lookup k xs of
       Just refv => writeIORef refv v
       Nothing => do
         kv <- pure $ (k, !(newIORef v))
         writeIOArray tbl.content ix (kv::xs)
         modifyIORef tbl.count S


export delete : (Hashable k, Eq k, HasIO io) => k -> IOHashtable k v -> io ()
delete k tbl = do
  let ix = restrict (ubound tbl.content) (cast $ saltedHash 0xdeadbeef k)
  readIOArray tbl.content ix
    >>= writeIOArray tbl.content ix . filter (\(xx, _) => k /= xx)


export contains : (Hashable k, Eq k, HasIO io) => k -> IOHashtable k v -> io Bool
contains k tbl = do
  let ix = restrict (ubound tbl.content) (cast $ saltedHash 0xdeadbeef k)
  xs <- readIOArray tbl.content ix
  pure $ maybe False (const True) $ lookup k xs


export lookup : (Hashable k, Eq k, HasIO io) => k -> IOHashtable k v -> io (Maybe v)
lookup k tbl = do
  let ix = restrict (ubound tbl.content) (cast $ saltedHash 0xdeadbeef k)
  xs <- readIOArray tbl.content ix
  maybe (pure Nothing) (\v' => pure $ Just !(readIORef v')) $ lookup k xs


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
