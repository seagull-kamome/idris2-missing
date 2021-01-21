||| Class definition of Bitset
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Bitset.Classes

%default total

public export
interface Set e t where
  insert : e -> t -> t
  remove : e -> t -> t
  lookup : e -> t -> Bool



