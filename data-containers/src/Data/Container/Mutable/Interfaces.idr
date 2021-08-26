||| Infaces of mutable container
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Container.Mutable.Interfaces

%default total

-- --------------------------------------------------------------------------
{-
namespace Stack
  public export
  interface MutableStack m (a:Type -> Type) where
    null  : a t -> m Bool
    length : a t -> m Nat
    push  : t -> a t -> m ()
    pop   : a t -> m (Maybe t)



namespace Queue
  public export
  interface MutableQueue m (a:Type -> Type) where
    null   : a t -> m Bool
    length : a t -> m Nat
    push   : t -> a t -> m Bool
    pull   : a t -> m (Maybe t)



namespace Map
  public export
  interface MutableMap m (a: Type -> Type) k | a where
    null   : a v -> m Bool
    length : a v -> m Nat
    contains : k -> a v -> m Bool
    insert : k -> v -> a v -> m Bool
    lookup : k -> a v -> m (Maybe v)
    delete : k -> a v -> m Bool


namespace Set
  public export
  interface MutableSet m (a:Type) k | a where
    null : a -> m Bool
    length : a -> m Bool
    contains : k -> a -> m Bool
    insert : k -> a -> m Bool
    delete : k -> a -> m Bool

-}

-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
