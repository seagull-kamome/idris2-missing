||| mutable array
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Container.Mutable.Array


import Data.IOArray.Prims as Prim

import Decidable.Equality
import Data.Fin
import Data.So

%default total

-- --------------------------------------------------------------------------

||| IOArray0 is same to Data.IOArray.Prims.ArrayData. But it's type-safe
||| on index-access.
export
data IOArray0 : (capacity:Nat) -> Type -> Type where
  MkIOArray0 : Prim.ArrayData t -> IOArray0 (S ubound) t


export newIOArray0 : HasIO io => (capacity:Nat)
    -> {auto 0 capacity_isnt_zero: So (capacity /= 0)}
    -> t
    -> io (IOArray0 capacity t)
newIOArray0 c@(S _) x = 
  pure $ MkIOArray0 !(primIO $ Prim.prim__newArray (cast c) x)



namespace Zero

  export %inline readIOArray : HasIO io => Fin c -> IOArray0 c t -> io t
  readIOArray i (MkIOArray0 xs)
    = primIO $ Prim.prim__arrayGet xs (cast $ finToNat i)

  export %inline writeIOArray : HasIO io => Fin c -> t -> IOArray0 c t -> io ()
  writeIOArray i x (MkIOArray0 xs)
    = primIO $ Prim.prim__arraySet xs (cast $ finToNat i) x




  namespace List

    export writeIOArray : HasIO io
        => {c:Nat} -> (from:Fin c) -> List t -> IOArray0 c t -> io ()
    writeIOArray _ [] _ = pure ()
    writeIOArray {c=c} i (x::xs) ys = do
      writeIOArray i x ys
      case natToFin (S $ finToNat i) c of
           Nothing => pure ()
           Just i' => writeIOArray i' xs ys


  export fromList : HasIO io
      => (xs:List t)
      -> io (Maybe (IOArray0 (length xs) t))
  fromList [] = pure Nothing
  fromList (x::xs) = do
    arr <- newIOArray0 (S $ length xs) x
    writeIOArray 0 xs arr
    pure $ Just arr


  export toList : HasIO io => {n:_} -> IOArray0 (S n) t -> io (List t)
  toList {n=n} xs = go last []
    where
      go : Fin (S n) -> List t -> io (List t)
      go i ys = do
        y <- readIOArray i xs
        case i of
             FZ    => pure  (y::ys)
             FS i' => assert_total $ go (weaken i') (y::ys)





-- --------------------------------------------------------------------------

||| IOArray has runtime boundary check
export
record IOArray t where
  constructor MkIOArray
  ub : Nat
  content : IOArray0 (S ub) t

export %inline ubound : IOArray t -> Nat
ubound xs = xs.ub

export %inline capacity : IOArray t -> Nat
capacity xs = S $ ubound xs

export capacity_is_ubound_plus1 : forall xs. (S (ubound xs)) = capacity xs
capacity_is_ubound_plus1 = Refl


export newIOArray : HasIO io
    => (capacity:Nat) -> {auto capacity_isnt_zero: So (capacity /= 0)}
    -> t
    -> io (IOArray t)
newIOArray capacity@(S u) x = pure $ MkIOArray u !(newIOArray0 capacity x)


namespace Fin
  export %inline readIOArray : HasIO io => (xs:IOArray t) -> Fin (S $ ubound xs) -> io t
  readIOArray xs i = pure $ !(readIOArray i xs.content)

  export %inline writeIOArray : HasIO io => (xs:IOArray t) -> Fin (S $ ubound xs) -> t -> io ()
  writeIOArray xs i x = writeIOArray i x xs.content

  export %inline restrict : (xs:IOArray t) -> Integer -> Fin (S $ ubound xs)
  restrict xs i = restrict (ubound xs) i



namespace Nat
  export readIOArray : HasIO io => Nat -> IOArray t -> io (Maybe t)
  readIOArray i xs with (integerToFin (cast i) (capacity xs))
    readIOArray i xs | Just i' = pure $ Just $ !(readIOArray i' xs.content)
    readIOArray i xs | Nothing = pure Nothing

  export writeIOArray : HasIO io => Nat -> t -> IOArray t -> io Bool
  writeIOArray i x xs with (integerToFin (cast i) (capacity xs))
    writeIOArray i x xs | Just i' = writeIOArray i' x xs.content >> pure True
    writeIOArray i x xs | Nothing = pure False


namespace List
  export fromList : HasIO io => List t -> io (Maybe (IOArray t))
  fromList [] = pure Nothing
  fromList (x::xs) = do
    arr <- newIOArray (S $ length xs) x
    writeIOArray arr 0 x
    pure $ Just arr 


  export toList : HasIO io => IOArray t -> io (List t)
  toList xs = go last []
    where
      go : Fin (S $ ubound xs) -> List t -> io (List t)
      go i ys = do
        y <- readIOArray xs i
        case i of
             FZ    => pure  (y::ys)
             FS i' => assert_total $ go (weaken i') (y::ys)




-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
