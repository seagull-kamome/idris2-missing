module Data.Bitset.Classes

%default total

public export
interface Set e t where
  insert : e -> t -> t
  remove : e -> t -> t
  lookup : e -> t -> Bool



