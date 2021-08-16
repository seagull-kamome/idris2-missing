||| Interfaces of basic containers.
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Container.Interfaces

import Data.Maybe

%default total

-- --------------------------------------------------------------------------

namespace Immutable

  public export
  interface Container a where
    0 KeyTy : Type
    null     : a -> Bool
    count    : a -> Nat
    contains : KeyTy -> a -> Bool
    delete   : KeyTy -> a -> a

  namespace Set
    public export
    interface Container a => Set a where
      0 KeyTy : Type
      insert   : KeyTy -> a -> a

  namespace Map
    public export
    interface Container a => Map a where
      0 KeyTy : Type
      0 ValTy : Type
      lookup   : KeyTy -> a -> Maybe ValTy
      insert   : KeyTy -> ValTy -> a -> a


-- --------------------------------------------------------------------------

namespace Mutable

  public export
  interface MutableContainer m a where
    0 KeyTy : Type
    null     : a -> m Bool
    count    : a -> m Nat
    contains : KeyTy -> a -> m Bool
    delete   : KeyTy -> a -> m ()

  namespace Set
    public export
    interface MutableContainer m a => MutableSet m a where
      0 KeyTy : Type
      insert   : KeyTy -> a -> m ()

  namespace Map
    public export
    interface MutableContainer m a => MutableMap m a where
      0 KeyTy : Type
      0 ValTy : Type
      lookup   : KeyTy -> a -> m (Maybe ValTy)
      insert   : KeyTy -> ValTy -> a -> m ()


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
