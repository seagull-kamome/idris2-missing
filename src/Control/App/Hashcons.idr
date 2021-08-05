||| Hashconsing App
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Control.App.Hashcons

import Control.App
import Data.So

import Data.Hash
import Data.Hashtable as H

%default total

-- ---------------------------------------------------------------------------

data FreshCounter : Type where
data Cache : Type -> Type where
export data HC t = MkHC (t, Int)

export %inline unintern : HC t -> t
unintern (MkHC (x, _)) = x

Eq (HC t) where MkHC (_, l) == MkHC (_, r) = l == r
Ord t => Ord (HC t) where
  compare (MkHC (l, _)) (MkHC (r, _)) = compare l r
  (MkHC (l, _)) < (MkHC (r, _)) = l > r
  (MkHC (l, _)) > (MkHC (r, _)) = l < r
  (MkHC (l, _)) <= (MkHC (r, _)) = l >= r
  (MkHC (l, _)) >= (MkHC (r, _)) = l <= r


public export
interface Hashcons t es where
  intern : t -> App {l} es (HC t)


public export partial
(Eq t, Hashable t,
 State (Cache t) (Int, IOHashtable t Int) es, PrimIO es)
 => Hashcons t es where
  intern k = do
    (fresh, cache) <- get (Cache t)
    x <- primIO $ H.lookup' k cache
    case x of
         Just x' => pure $ MkHC x'
         Nothing => do
           x' <- primIO $ H.insert' k (fresh + 1) cache
           _ <- put (Cache t) (fresh + 1, cache)
           pure $ MkHC x'

export partial
withHashcons : (Hashcons t es, PrimIO es)
               => (cachesize:Int) -> {auto _:So (cachesize > 0)}
               -> App {l} es a -> App {l} es a
withHashcons cachesize prog = do
  cache <- primIO $ H.newHashtable {k=t} {v=Int} cachesize
  new {tag=Cache t} (0, cache) prog





