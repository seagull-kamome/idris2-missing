||| mutable array
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Container.Mutable.Array

import Data.IOArray.Prims as Prim
import Data.Fin
import Data.List
import Data.So

%default total

-- --------------------------------------------------------------------------

export IOArray0 : Nat -> Type -> Type
IOArray0 c t = Prim.ArrayData (Maybe t)

export newIOArray0 : HasIO io => {capacity:Nat}
    -> {auto 0 capacity_isnt_zero: So (capacity /= Z)}
    -> io (IOArray0 capacity t)
newIOArray0 {capacity=(S c)} = primIO $ Prim.prim__newArray (cast c + 1) Nothing


export %inline readArray0 : HasIO io => Fin c -> IOArray0 c t -> io (Maybe t)
readArray0 i xs = primIO $ Prim.prim__arrayGet xs (fromInteger $ cast i)


export %inline writeArray0' : HasIO io => Fin c -> Maybe t -> IOArray0 c t -> io ()
writeArray0' i x xs = primIO $ Prim.prim__arraySet xs (fromInteger $ cast i) x


export %inline writeArray0 : HasIO io => Fin c -> t -> IOArray0 c t -> io ()
writeArray0 i x xs = writeArray0' i (Just x) xs


export %inline deleteArray0 : HasIO io => Fin c -> IOArray0 c t -> io ()
deleteArray0 i xs = writeArray0' i Nothing xs





-- --------------------------------------------------------------------------

export
data IOArray : Type -> Type where
  MkIOArray : (u:Nat) -> IOArray0 (S u) t -> IOArray t


export %inline capacity : IOArray t -> Nat
capacity (MkIOArray u _) = S u


export %inline lbound : IOArray t -> Nat
lbound _ = 0

export %inline ubound : IOArray t -> Nat
ubound (MkIOArray u _) = u


export %inline newIOArray : HasIO io
    => (capacity:Nat) -> {0 capacity_isnt_zero: So (capacity /= Z)}
    -> io (IOArray t)
newIOArray capacity@(S u) = pure $ MkIOArray u !(primIO $ prim__newArray (cast capacity) Nothing)

export %inline readIOArray : HasIO io => (i:Fin (capacity xs)) -> (xs:IOArray t) -> io (Maybe t)
readIOArray i (MkIOArray u xs) = primIO $ prim__arrayGet xs (fromInteger $ cast i)


export %inline writeIOArray' : HasIO io => Maybe t -> (i:Fin (capacity xs)) -> (xs:IOArray t) -> io ()
writeIOArray' x i (MkIOArray u xs) = primIO $ prim__arraySet xs (fromInteger $ cast i) x


export %inline writeIOArray : HasIO io => t -> (i:Fin (capacity xs)) -> (xs:IOArray t) -> io ()
writeIOArray x i xs = writeIOArray' (Just x) i xs


export %inline deleteIOArray : HasIO io => (i:Fin (capacity xs)) -> (xs:IOArray t) -> io ()
deleteIOArray i xs = writeIOArray' Nothing i xs



-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
