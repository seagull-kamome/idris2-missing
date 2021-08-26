||| mutable array
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Container.Mutable.RingQueue

import Data.Container.Mutable.Array

import Data.Nat
import Data.IORef
import Data.Fin
import Data.So

%default total

-- --------------------------------------------------------------------------

export
record IORingQueue t where
  constructor MkIORingQueue
  content : IOArray (Maybe t)
  loc : IORef (Nat, Nat)


export newIORingQueue : HasIO io
  => (cap:Nat) -> {auto 0 capacity_isnt_zer: So (cap /= 0)}
  -> io (IORingQueue t)
newIORingQueue cap@(S c) = pure $ MkIORingQueue !(newIOArray cap Nothing) !(newIORef (0, 0))


export null : HasIO io => IORingQueue a -> io Bool
null q = readIORef q.loc >>= pure . (0 ==) . snd


export length : HasIO io => IORingQueue a -> io Nat
length q = readIORef q.loc >>= pure . snd


export push : HasIO io => a -> IORingQueue a -> io Bool
push x q = do
  (n, m) <- readIORef q.loc
  if m >= capacity q.content
     then pure False
     else do
       writeIOArray q.content (restrict q.content (cast $ n + m)) (Just x)
       writeIORef q.loc (n, m + 1)
       pure True


export pull : HasIO io => IORingQueue a -> io (Maybe a)
pull q = do
  (n, (S m)) <- readIORef q.loc
   | (_, Z) => pure Nothing
  writeIORef q.loc ((n + 1) `mod` (capacity q.content), m)
  let i = restrict q.content $ cast n
  r <- readIOArray q.content i
  writeIOArray q.content i Nothing
  pure r



{-
export
HasIO io => MutableQueue io IORingQueue where
    null q = readIORef q.loc >>= pure . (0 ==) . snd
    length q = readIORef q.loc >>= pure . snd

    push x q = do
      (n, m) <- readIORef q.loc
      if m >= capacity q.content
         then pure False
         else do
           writeIOArray q.content (restrict q.content (cast $ n + m)) (Just x)
           writeIORef q.loc (n, m + 1)
           pure True

    pull q = do
      (n, (S m)) <- readIORef q.loc
        | (_, Z) => pure Nothing
      writeIORef q.loc ((n + 1) `mod` (capacity q.content), m)
      let i = restrict q.content $ cast n
      r <- readIOArray q.content i
      writeIOArray q.content i Nothing
      pure r

-}

-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
