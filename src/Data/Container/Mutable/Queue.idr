||| mutable queue
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Container.Mutable.Queue

import Data.List
import Data.IOArray
import Data.IORef
import System.Info

%default total

-- --------------------------------------------------------------------------

public export
interface MutableQueue m a where
  0 ValTy : Type
  null  : a -> m Bool
  count : a -> m Nat
  enqueue  : ValTy -> a -> m ()
  dequeue  : a -> m (Maybe ValTy)

-- --------------------------------------------------------------------------
-- Backend specific mutable queue


data PrimIOQueue : Type -> Type where [external]

%foreign "javascript:lambda:() => new Array()"
prim__ioqueue_new : PrimIO (PrimIOQueue t)

%foreign "javascript:lambda:(x, xs) => xs.push(x)"
prim__ioqueue_enqueue : t -> PrimIOQueue t -> PrimIO ()

%foreign "javascript:lambda:(d, f, xs) => { const x = xs.shift(); return (x === undefined)? d : f(x); }"
prim__ioqueue_dequeue : a -> (t -> a) -> PrimIOQueue t -> PrimIO a

%foreign "javascript:lambda:(xs) => xs.length == 0"
prim__ioqueue_null : PrimIOQueue t -> PrimIO Bool

%foreign "javascript:lambda:(xs) => xs.length"
prim__ioqueue_count : PrimIOQueue t -> PrimIO Int



%inline export newPrimIOQueue : HasIO io => io (PrimIOQueue t)
newPrimIOQueue = primIO $ prim__ioqueue_new


export
HasIO m => MutableQueue m (PrimIOQueue t) where
  ValTy     = t
  null xs   = primIO $ prim__ioqueue_null xs
  count xs  = (primIO $ prim__ioqueue_count xs) >>= pure . fromInteger . cast
  enqueue x xs = primIO $ prim__ioqueue_enqueue x xs
  dequeue xs   = primIO $ prim__ioqueue_dequeue Nothing Just xs


-- --------------------------------------------------------------------------

%inline public export
hasNativeIOQueue : Bool
hasNativeIOQueue with (codegen)
  hasNativeIOQueue | "javascript" = True
  hasNativeIOQueue | _            = False


-- --------------------------------------------------------------------------
-- Generic IOArray based mutable queue

export
record IOArrayQueue t where
  constructor MkIOArrayQueue
  allocsize : IORef Nat
  pages : IORef (Nat, Nat, IOArray t, List (IOArray t), List (IOArray t))


export newIOArrayQueue : HasIO io => io (IOArrayQueue t)
newIOArrayQueue = do
  xs <- newArray 512
  pure $ MkIOArrayQueue !(newIORef 512) !(newIORef (0, 0, xs, [], []))


export
HasIO m => MutableQueue m (IOArrayQueue t) where
  ValTy     = t
  null xs   = do
    (n, m, ys, zs, ws) <- readIORef xs.pages
    pure $ n == m && null zs && null ws
  count xs  = do
    (n, m, ys, zs, ws) <- readIORef xs.pages
    pure $ fromInteger $ cast $ sum (map Data.IOArray.max zs) + sum (map Data.IOArray.max ws) + (cast n) - (cast m)
  enqueue x xs = do
    (n, m, ys, zs, ws) <- readIORef xs.pages
    if cast n < Data.IOArray.max ys
       then writeArray ys (cast n) x >> writeIORef xs.pages ((n + 1), m, ys, zs, ws)
       else do
         ac <- readIORef xs.allocsize
         ys' <- newArray (cast ac)
         writeArray ys' 0 x
         writeIORef xs.pages (1, m, ys', (ys::zs), ws)
  dequeue xs    = do 
    (n, m, ys, zss, wss) <- readIORef xs.pages
    case wss of
         (ws::wss') => do
           if (cast $ m + 1) >= Data.IOArray.max ws
              then writeIORef xs.pages (n, 0, ys, zss, wss')
              else writeIORef xs.pages (n, m + 1, ys, zss, wss)
           readArray ws (cast m)
         [] => case reverse zss of
                    zss'@(zs::_) => do
                      writeIORef xs.pages (n, 1, ys, [], zss')
                      readArray zs 0
                    [] => if n == 0
                             then pure Nothing
                             else do
                               if m + 1 >= n
                                  then writeIORef xs.pages (0, 0, ys, [], [])
                                  else writeIORef xs.pages (n, m + 1, ys, [], [])
                               readArray ys (cast m)

-- --------------------------------------------------------------------------

%inline public export IOQueue : Type -> Type
IOQueue t  with (hasNativeIOQueue)
  IOQueue t | True  = PrimIOQueue t
  IOQueue t | False = IOArrayQueue t


%inline public export newIOQueue : HasIO io => io (IOQueue t)
newIOQueue with (hasNativeIOQueue)
  newIOQueue | True  = newPrimIOQueue
  newIOQueue | False = newIOArrayQueue

-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
