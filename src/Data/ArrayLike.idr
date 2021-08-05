module Data.ArrayLike

import Decidable.Equality

%default total

infix 7 !!

-- ---------------------------------------------------------------------------
--

public export
interface ArrayLike arr where
  elemType : Type
  length : arr -> Int
  findByV : (elemType -> Bool) -> (xs:arr)
         -> Maybe (i:Int ** (0 <= i = True, i < length xs = True))
  findrByV : (elemType -> Bool) -> (xs:arr)
          -> Maybe (i:Int ** (0 <= i = True, i < length xs = True))

  (!!) : (xs:arr) -> (i:Int)            -- WHY THE HELL THIS CANN'T IMPLEMENT
      -> {auto _: 0 <= i = True }
      -> {auto _: i < length xs = True }
      -> elemType

  findBy : (elemType -> Bool) -> (xs:arr) -> Maybe Int
  findBy f xs with (findByV f xs)
    findBy f xs | Nothing = Nothing
    findBy f xs | Just (i ** _) = Just i

  findrBy : (elemType -> Bool) -> (xs:arr) -> Maybe Int
  findrBy f xs with (findrByV f xs)
    findrBy f xs | Nothing = Nothing
    findrBy f xs | Just (i ** _) = Just i

at' : ArrayLike arr => Int -> arr -> Maybe (elemType{arr=arr})
at' i xs with (decEq (0 <= i) True, decEq (i < length xs) True)
  at' i xs | (Yes pr0, Yes pr1) = Just $ xs !! i
  at' i xs | _ = Nothing

-- ---------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
