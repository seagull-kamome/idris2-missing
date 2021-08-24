||| Infaces of mutable container
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Container.Mutable.Interfaces

%default total

-- --------------------------------------------------------------------------

public export
interface MutableQueue m (a:Type -> Type) where
  null  : a t -> m Bool
  count : a t -> m Nat
  enqueue  : t -> a t -> m Bool
  dequeue  : a t -> m (Maybe t)


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
