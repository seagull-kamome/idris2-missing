||| mutable queue
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Container.Mutable.Queue

import Data.Container.Mutable.Array
import public Data.Container.Mutable.Interfaces

import Data.Fin
import Data.List
import Data.IORef
import System.Info
import Data.So

%default total

-- --------------------------------------------------------------------------
-- Backend specific mutable queue

namespace Prim

  data PrimIOQueue : Type -> Type where [external]

  %foreign "javascript:lambda:() => new Array()"
  prim__ioqueue_new : PrimIO (PrimIOQueue t)

  %foreign "javascript:lambda:(x, xs) => xs.push(x)"
  prim__ioqueue_push : t -> PrimIOQueue t -> PrimIO ()

  %foreign "javascript:lambda:(d, f, xs) => { const x = xs.shift(); return (x === undefined)? d : f(x); }"
  prim__ioqueue_pull : a -> (t -> a) -> PrimIOQueue t -> PrimIO a

  %foreign "javascript:lambda:(xs) => xs.length == 0"
  prim__ioqueue_null : PrimIOQueue t -> PrimIO Bool

  %foreign "javascript:lambda:(xs) => xs.length"
  prim__ioqueue_length : PrimIOQueue t -> PrimIO Int



  %inline export newPrimIOQueue : HasIO io => io (PrimIOQueue t)
  newPrimIOQueue = primIO $ prim__ioqueue_new


  export null : HasIO io => PrimIOQueue a -> io Bool
  null xs   = primIO $ prim__ioqueue_null xs


  export length : HasIO io => PrimIOQueue a -> io Nat
  length xs  = (primIO $ prim__ioqueue_length xs) >>= pure . fromInteger . cast


  export push : HasIO io => a -> PrimIOQueue a -> io Bool
  push x xs = (primIO $ prim__ioqueue_push x xs) >> pure True


  export pull : HasIO io => PrimIOQueue a -> io (Maybe a)
  pull xs   = primIO $ prim__ioqueue_pull Nothing Just xs

{-
  export
  HasIO io => MutableQueue io PrimIOQueue where
    null xs   = primIO $ prim__ioqueue_null xs
    length xs  = (primIO $ prim__ioqueue_length xs) >>= pure . fromInteger . cast
    push x xs = (primIO $ prim__ioqueue_push x xs) >> pure True
    pull xs   = primIO $ prim__ioqueue_pull Nothing Just xs
 -}

-- --------------------------------------------------------------------------

%inline public export
hasNativeIOQueue : Bool
hasNativeIOQueue with (codegen)
  hasNativeIOQueue | "javascript" = True
  hasNativeIOQueue | _            = False


-- --------------------------------------------------------------------------
-- Generic IOArray based mutable queue

namespace Generic

  record Bin t where
    constructor MkBin
    content : IOArray (Maybe t)
    link: IORef (Maybe $ Bin t)

  %inline InitialBinSize : Nat
  InitialBinSize = 511

  export
  record IOArrayQueue t where
    constructor MkIOArrayQueue
    allocsize : IORef Nat
    content : IORef (Nat, Nat, Bin t, Nat, Bin t)


  export newIOArrayQueue : HasIO io => io (IOArrayQueue t)
  newIOArrayQueue = do
    bin <- pure $ MkBin !(newIOArray (S InitialBinSize) Nothing) !(newIORef Nothing)
    pure $ MkIOArrayQueue !(newIORef InitialBinSize)
                        !(newIORef (0, 0, bin, 0, bin))

  export null : HasIO io => IOArrayQueue t -> io Bool
  null q   = readIORef q.content >>= pure . (0 ==) . fst


  export length : HasIO io => IOArrayQueue a -> io Nat
  length q  = readIORef q.content >>= pure . fst


  export push : HasIO io => a -> IOArrayQueue a -> io Bool
  push x q = do
    (c, n, b0, m, b1) <- readIORef q.content
    case natToFin n (S $ ubound b0.content) of
      Just n' => do
        writeIOArray b0.content n' (Just x)
        writeIORef q.content (S c, S n, b0, m, b1)
      Nothing => do
        l <- readIORef q.allocsize
        b2 <- pure $ MkBin !(newIOArray (S l) Nothing) !(newIORef Nothing)
        writeIORef b0.link (Just b2)
        writeIOArray b2.content 0 (Just x)
        writeIORef q.content (S c, 1, b2, m, b1)
    pure True


  export pull : HasIO io => IOArrayQueue a -> io (Maybe a)
  pull q = do
    ((S c), n, b0, m, b1) <- readIORef q.content
      | (Z, _, _, _, _) => pure Nothing
    if c == 0
       then writeIORef q.content (0, 0, b0, 0, b0)
       else if m == ubound b1.content
               then do
                 Just b2 <- readIORef b1.link
                   | Nothing => writeIORef q.content (0, 0, b0, 0, b0) -- never happens
                 writeIORef q.content (c, n, b0, 0, b2)
               else
                 writeIORef q.content (c, n, b0, m + 1, b1)
    let m' = restrict b1.content $ cast m
    r <- readIOArray b1.content m' -- always Just
    writeIOArray b1.content m' Nothing
    pure r



{-
export
HasIO io => MutableQueue io IOArrayQueue where
  null q   = readIORef q.content >>= pure . (0 ==) . fst
  length q  = readIORef q.content >>= pure . fst

  push x q = do
    (c, n, b0, m, b1) <- readIORef q.content
    case natToFin n (S $ ubound b0.content) of
      Just n' => do
        writeIOArray b0.content n' (Just x)
        writeIORef q.content (S c, S n, b0, m, b1)
      Nothing => do
        l <- readIORef q.allocsize
        b2 <- pure $ MkBin !(newIOArray (S l) Nothing) !(newIORef Nothing)
        writeIORef b0.link (Just b2)
        writeIOArray b2.content 0 (Just x)
        writeIORef q.content (S c, 1, b2, m, b1)
    pure True

  pull q = do
    ((S c), n, b0, m, b1) <- readIORef q.content
      | (Z, _, _, _, _) => pure Nothing
    if c == 0
       then writeIORef q.content (0, 0, b0, 0, b0)
       else if m == ubound b1.content
               then do
                 Just b2 <- readIORef b1.link
                   | Nothing => writeIORef q.content (0, 0, b0, 0, b0) -- never happens
                 writeIORef q.content (c, n, b0, 0, b2)
               else
                 writeIORef q.content (c, n, b0, m + 1, b1)
    let m' = restrict b1.content $ cast m
    r <- readIOArray b1.content m' -- always Just
    writeIOArray b1.content m' Nothing
    pure r
    -}

-- --------------------------------------------------------------------------

{-
%inline public export IOQueue : Type -> Type
IOQueue t  with (hasNativeIOQueue)
  IOQueue t | True  = PrimIOQueue t
  IOQueue t | False = IOArrayQueue t


%inline public export newIOQueue : HasIO io => io (IOQueue t)
newIOQueue with (hasNativeIOQueue)
  newIOQueue | True  = newPrimIOQueue
  newIOQueue | False = newIOArrayQueue
  -}
-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
