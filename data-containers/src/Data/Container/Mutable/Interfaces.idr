||| Infaces of mutable container
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Container.Mutable.Interfaces

%default total

-- --------------------------------------------------------------------------

public export
interface MutableQueue m a where
  0 ValTy : Type
  null  : a -> m Bool
  count : a -> m Nat
  enqueue  : ValTy -> a -> m Bool
  dequeue  : a -> m (Maybe ValTy)


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
