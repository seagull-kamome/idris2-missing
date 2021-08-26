||| Simple mutable hashtable.
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Data.Container.Mutable.Hashtable

import Data.Container.Mutable.Array
import public Data.Container.Mutable.Interfaces
import Data.Hash

import Data.Maybe
import Data.Fin
import Data.So
import Data.List
import Data.IORef


%default total

-- --------------------------------------------------------------------------

export data IOHashtable k v
  = MkIOHashtable (IORef Nat) (IOArray (List (k, IORef v)))

-- --------------------------------------------------------------------------

export newIOHashtable : HasIO io
    => (capacity:Nat)
    -> {auto 0 capacity_isnt_zero: So (capacity /= 0)}
    -> io (IOHashtable k v)
newIOHashtable capacity@(S _)
  = pure $ MkIOHashtable !(newIORef 0) !(newIOArray capacity [])

-- --------------------------------------------------------------------------

export null : (HasIO io, Eq k, Hashable k) => IOHashtable k v -> io Bool
null (MkIOHashtable c _) = pure $ !(readIORef c) == 0


export length : (HasIO io, Eq k, Hashable k) => IOHashtable k v -> io Nat
length (MkIOHashtable c _) = readIORef c


export contains : (HasIO io, Eq k, Hashable k) => k -> IOHashtable k v -> io Bool
contains k (MkIOHashtable _ arr) = do
  let ix = restrict (ubound arr) (cast $ saltedHash 0xdeadbeef k)
  xs <- readIOArray arr ix
  pure $ maybe False (const True) $ lookup k xs


export insert : (HasIO io, Eq k, Hashable k) => k -> v -> IOHashtable k v -> io Bool
insert k v (MkIOHashtable c arr) = do
  let ix = restrict (ubound arr) (cast $ saltedHash 0xdeadbeef k)
  xs <- readIOArray arr ix
  case lookup k xs of
       Just refv => writeIORef refv v
       Nothing => do
         kv <- pure $ (k, !(newIORef v))
         writeIOArray arr ix (kv::xs)
         modifyIORef c S
  pure True


export lookup : (HasIO io, Eq k, Hashable k) => k -> IOHashtable k v -> io (Maybe v)
lookup k (MkIOHashtable _ arr) = do
  let ix = restrict (ubound arr) (cast $ saltedHash 0xdeadbeef k)
  xs <- readIOArray arr ix
  maybe (pure Nothing) (\v' => pure $ Just !(readIORef v')) $ lookup k xs


export delete : (HasIO io, Eq k, Hashable k) => k -> IOHashtable k v -> io Bool
delete k (MkIOHashtable c arr) = do
  let ix = restrict (ubound arr) (cast $ saltedHash 0xdeadbeef k)
  xs <- readIOArray arr ix
  let xs' = filter ((k /=) . fst) xs
      d = cast (length xs) - cast (length xs')
  writeIOArray arr ix xs'
  modifyIORef c (\c' => fromInteger $ cast c' - d)
  pure $ d /= 0



{-
export
(HasIO io, Eq k, Hashable k) => MutableMap io (IOHashtable k) where

  null (MkIOHashtable c _) = pure $ !(readIORef c) == 0
  length (MkIOHashtable c _) = readIORef c

  contains k (MkIOHashtable _ arr) = do
    let ix = restrict (ubound arr) (cast $ saltedHash 0xdeadbeef k)
    xs <- readIOArray arr ix
    pure $ maybe False (const True) $ lookup k xs

  insert k v (MkIOHashtable c arr) = do
    let ix = restrict (ubound arr) (cast $ saltedHash 0xdeadbeef k)
    xs <- readIOArray arr ix
    case lookup k xs of
         Just refv => writeIORef refv v >> pure True
         Nothing => do
           kv <- pure $ (k, !(newIORef v))
           writeIOArray arr ix (kv::xs)
           modifyIORef c S
           pure True

  lookup k (MkIOHashtable _ arr) = do
    let ix = restrict (ubound arr) (cast $ saltedHash 0xdeadbeef k)
    xs <- readIOArray arr ix
    maybe (pure Nothing) (\v' => pure $ Just !(readIORef v')) $ lookup k xs

  delete k (MkIOHashtable c arr) = do
    let ix = restrict (ubound arr) (cast $ saltedHash 0xdeadbeef k)
    xs <- readIOArray arr ix
    let xs' = filter ((k /=) . fst) xs
        d = cast (length xs) - cast (length xs')
    writeIOArray arr ix xs'
    modifyIORef c (+ fromInteger d)
    pure $ d /= 0

-}

-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
