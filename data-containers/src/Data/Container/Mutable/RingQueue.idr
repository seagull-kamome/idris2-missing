||| mutable array
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Container.Mutable.RingQueue

import Data.Container.Mutable.Array
import Data.Container.Mutable.Interfaces

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


export
HasIO io => MutableQueue io (IORingQueue t) where
    ValTy = t
    null q = pure $ !(count q) == 0
    count q = readIORef q.loc >>= pure . snd

    enqueue x q = do
      (n, m) <- readIORef q.loc
      if m >= capacity q.content
         then pure False
         else do
           writeIOArray q.content (restrict q.content (cast $ n + m)) (Just x)
           writeIORef q.loc (n, m + 1)
           pure True

    dequeue q = do
      (n, (S m)) <- readIORef q.loc
        | (_, Z) => pure Nothing
      writeIORef q.loc ((n + 1) `mod` (capacity q.content), m)
      readIOArray q.content (restrict q.content $ cast n)



-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
