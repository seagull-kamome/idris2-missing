||| mutable stack
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Container.Mutable.Stack

import Data.List1
import Data.IOArray
import Data.IORef
import System.Info

%default total

-- --------------------------------------------------------------------------

public export
interface MutableStack m a where
  0 ValTy : Type
  null  : a -> m Bool
  count : a -> m Nat
  push  : ValTy -> a -> m ()
  pop   : a -> m (Maybe ValTy)

-- --------------------------------------------------------------------------
-- Backend specific mutable stack


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
prim__iostack_count : PrimIOStack t -> PrimIO Int



%inline export newPrimIOStack : HasIO io => io (PrimIOStack t)
newPrimIOStack = primIO $ prim__iostack_new


export
HasIO m => MutableStack m (PrimIOStack t) where
  ValTy     = t
  null xs   = primIO $ prim__iostack_null xs
  count xs  = (primIO $ prim__iostack_count xs) >>= pure . fromInteger . cast
  push x xs = primIO $ prim__iostack_push x xs
  pop xs    = primIO $ prim__iostack_pop Nothing Just xs


-- --------------------------------------------------------------------------

%inline public export
hasNativeIOStack : Bool
hasNativeIOStack with (codegen)
  hasNativeIOStack | "javascript" = True
  hasNativeIOStack | _            = False


-- --------------------------------------------------------------------------
-- Generic IOArray based mutable stack

export
record IOArrayStack t where
  constructor MkIOArrayStack
  allocsize : IORef Nat
  pages : IORef (Nat, List1 (IOArray t))


export newIOArrayStack : HasIO io => io (IOArrayStack t)
newIOArrayStack = do
  xs <- newArray 512
  pure $ MkIOArrayStack !(newIORef 512) !(newIORef (0, (xs:::[])))


export
HasIO m => MutableStack m (IOArrayStack t) where
  ValTy     = t
  null xs   = do
    (n, (y:::ys)) <- readIORef xs.pages
    pure $ n == 0 && null ys
  count xs  = do
    (n, (y:::ys)) <- readIORef xs.pages
    pure $ fromInteger $ cast $ sum (map Data.IOArray.max ys) + (cast n)
  push x xs = do
    (n, ys'@(y:::ys)) <- readIORef xs.pages
    if cast n < Data.IOArray.max y
       then writeArray y (cast n) x >> writeIORef xs.pages (n + 1, ys')
       else do
         -- update acclocation unit size.
         ac <- readIORef xs.allocsize
         let newac = if cast (Data.IOArray.max y) < natToInteger ac then ac else (ac + ac)
         writeIORef xs.allocsize newac
         -- allocate new page
         newpage <- newArray $ cast newac
         writeArray newpage 0 x
         writeIORef xs.pages (1, (newpage:::(y::ys)))
  pop xs    = do
    (n, ys'@(y:::ys)) <- readIORef xs.pages
    case n of
         S n' => writeIORef xs.pages (n', ys') >> readArray y (cast n')
         Z => case ys of
                   [] => pure Nothing
                   (y'::ys'') => do
                     let n' = Data.IOArray.max y' - 1
                     writeIORef xs.pages (fromInteger (cast n'), y':::ys'')
                     readArray y' n'

-- --------------------------------------------------------------------------

%inline public export IOStack : Type -> Type
IOStack t  with (hasNativeIOStack)
  IOStack t | True  = PrimIOStack t
  IOStack t | False = IOArrayStack t


%inline public export newIOStack : HasIO io => io (IOStack t)
newIOStack with (hasNativeIOStack)
  newIOStack | True  = newPrimIOStack
  newIOStack | False = newIOArrayStack

-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
