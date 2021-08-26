||| mutable stack
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Container.Mutable.Stack

import Data.Container.Mutable.Array

import Data.Fin
import Data.List1
import Data.IORef
import Data.So
import System.Info

%default total

-- --------------------------------------------------------------------------
-- Backend specific mutable stack

namespace Prim

  data PrimIOStack : Type -> Type where [external]

  %foreign "javascript:lambda:() => new Array()"
  prim__iostack_new : PrimIO (PrimIOStack t)

  %foreign "javascript:lambda:(x, xs) => xs.push(x)"
  prim__iostack_push : t -> PrimIOStack t -> PrimIO ()

  %foreign "javascript:lambda:(d, f, xs) => { const x = xs.pop(); return (x === undefined)? d : f(x); }"
  prim__iostack_pop : a -> (t -> a) -> PrimIOStack t -> PrimIO a

  %foreign "javascript:lambda:(xs) => xs.length == 0"
  prim__iostack_null : PrimIOStack t -> PrimIO Bool

  %foreign "javascript:lambda:(xs) => xs.length"
  prim__iostack_length : PrimIOStack t -> PrimIO Int



  %inline export newPrimIOStack : HasIO io => io (PrimIOStack t)
  newPrimIOStack = primIO $ prim__iostack_new

  export null : HasIO io => PrimIOStack t -> io Bool
  null xs   = primIO $ prim__iostack_null xs

  export length : HasIO io => PrimIOStack t -> io Nat
  length xs  = (primIO $ prim__iostack_length xs) >>= pure . fromInteger . cast

  export push : HasIO io => t -> PrimIOStack t -> io ()
  push x xs = primIO $ prim__iostack_push x xs

  export pop : HasIO io => PrimIOStack t -> io (Maybe t)
  pop xs    = primIO $ prim__iostack_pop Nothing Just xs

  {-
  export
  HasIO io => MutableStack io PrimIOStack where
    null xs   = primIO $ prim__iostack_null xs
    length xs  = (primIO $ prim__iostack_length xs) >>= pure . fromInteger . cast
    push x xs = primIO $ prim__iostack_push x xs
    pop xs    = primIO $ prim__iostack_pop Nothing Just xs
  -}

-- --------------------------------------------------------------------------

%inline public export
HasNativeIOStack : Bool
HasNativeIOStack with (codegen)
  HasNativeIOStack | "javascript" = True
  HasNativeIOStack | _            = False


-- --------------------------------------------------------------------------
-- Generic IOArray based mutable stack
namespace Generic

  export
  record IOArrayStack t where
    constructor MkIOArrayStack
    allocsize : IORef Nat
    pages : IORef (Nat, List1 (IOArray (Maybe t)))


  export newIOArrayStack : HasIO io => io (IOArrayStack t)
  newIOArrayStack = do
    let s = 511
    xs <- newIOArray (S s) Nothing
    pure $ MkIOArrayStack !(newIORef s) !(newIORef (0, (xs:::[])))


  export null : HasIO io => IOArrayStack t -> io Bool
  null xs   = do
    (n, (y:::ys)) <- readIORef xs.pages
    pure $ n == 0 && null ys


  export length : HasIO io => IOArrayStack t -> io Nat
  length xs  = do
    (n, (y:::ys)) <- readIORef xs.pages
    pure $ fromInteger $ cast $ sum (map capacity ys) + (cast n)

  export push : HasIO io => t -> IOArrayStack t -> io ()
  push x xs = do
    (n, ys'@(y:::ys)) <- readIORef xs.pages
    case natToFin n (S $ ubound y) of
         Just n' => writeIOArray y n' (Just x) >> writeIORef xs.pages (S n, ys')
         Nothing => do
           -- update acclocation unit size.
           ac <- readIORef xs.allocsize
           let newac = if capacity y < S ac then ac else S (ac + ac)
           writeIORef xs.allocsize newac

           -- allocate new page
           newpage <- newIOArray (S newac) Nothing
           writeIOArray newpage 0 (Just x)
           writeIORef xs.pages (1, (newpage:::(y::ys)))

  export pop : HasIO io => IOArrayStack t -> io (Maybe t)
  pop xs    = do
    (n, ys'@(y:::ys)) <- readIORef xs.pages
    case n of
         S n' => do
           writeIORef xs.pages (n', ys')
           let n'' = restrict y (cast n')
           r <- readIOArray y n''
           writeIOArray y n'' Nothing
           pure r

         Z => case ys of
                   [] => pure Nothing
                   (y'::ys'') => do
                     writeIORef xs.pages (ubound y', y':::ys'')
                     let l = restrict y' $ cast $ ubound y'
                     r <- readIOArray y' l
                     writeIOArray y' l Nothing
                     pure r


{-
export
HasIO io => MutableStack io IOArrayStack where
  null xs   = do
    (n, (y:::ys)) <- readIORef xs.pages
    pure $ n == 0 && null ys

  length xs  = do
    (n, (y:::ys)) <- readIORef xs.pages
    pure $ fromInteger $ cast $ sum (map capacity ys) + (cast n)

  push x xs = do
    (n, ys'@(y:::ys)) <- readIORef xs.pages
    case natToFin n (S $ ubound y) of
         Just n' => writeIOArray y n' (Just x) >> writeIORef xs.pages (S n, ys')
         Nothing => do
           -- update acclocation unit size.
           ac <- readIORef xs.allocsize
           let newac = if capacity y < S ac then ac else S (ac + ac)
           writeIORef xs.allocsize newac

           -- allocate new page
           newpage <- newIOArray (S newac) Nothing
           writeIOArray newpage 0 (Just x)
           writeIORef xs.pages (1, (newpage:::(y::ys)))

  pop xs    = do
    (n, ys'@(y:::ys)) <- readIORef xs.pages
    case n of
         S n' => do
           writeIORef xs.pages (n', ys')
           let n'' = restrict y (cast n')
           r <- readIOArray y n''
           writeIOArray y n'' Nothing
           pure r

         Z => case ys of
                   [] => pure Nothing
                   (y'::ys'') => do
                     writeIORef xs.pages (ubound y', y':::ys'')
                     let l = restrict y' $ cast $ ubound y'
                     r <- readIOArray y' l
                     writeIOArray y' l Nothing
                     pure r
-}

-- --------------------------------------------------------------------------

{- WONN'T WORK!!!!

public export %inline IOStack : Type -> Type
IOStack t with (HasNativeIOStack)
  IOStack t | True  = PrimIOStack t
  IOStack t | False = IOArrayStack t

public export %inline
newIOStack : HasIO io => io (IOStack t)
newIOStack with (HasNativeIOStack)
  newIOStack | True  = newPrimIOStack
  newIOStack | False = newIOArrayStack
{-
export
IOStackMutableStack : HasIO io => MutableStack io IOStack t
IOStackMutableStack with (hasNativeIOStack)
  IOStackMutableStack | True  = PrimIOStackMutableStack
  IOStackMutableStack | False = IOArrayStackMutableStack
  -}
-- public export
-- HasIO io => MutableStack io IOStack t using IOStackMutableStack where

-}
-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
