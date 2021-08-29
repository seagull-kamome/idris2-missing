||| immutable queue
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Container.Immutable.Queue

import Data.List
import Data.So

%default total

-- --------------------------------------------------------------------------

export data Queue t = MkQueue (List t) (List t)

export newQueue : Queue t
newQueue = MkQueue [] []


export null : Queue t -> Bool
null (MkQueue [] []) = True
null _ = False


export length : Queue t -> Nat
length (MkQueue ys zs) = length ys + length zs


export push : t -> Queue t -> Queue t
push x (MkQueue ys zs) = MkQueue (x::ys) zs


export pull : Queue t -> (Maybe t, Queue t)
pull q@(MkQueue ys []) =
  case reverse ys of
       []      => (Nothing, q)
       (y::ys) => (Just y, MkQueue [] ys)
pull (MkQueue ys (z::zs)) = (Just z, MkQueue ys zs)


-- --------------------------------------------------------------------------

export null_length_is_zero : {x:Queue t} -> (pr: So (null x)) -> length x = 0
null_length_is_zero = ?lhs

{-
export push_pop_doesnt_change_length : 
  {xs:Queue t} -> (pull (push x xs) == (x, ys) /\ xs = ys)
-}



-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
