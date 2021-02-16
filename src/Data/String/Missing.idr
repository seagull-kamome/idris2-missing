module Data.String.Missing

import Data.Strings
import Data.String.Extra
import Decidable.Equality

import Data.ArrayLike

%default total

-- ---------------------------------------------------------------------------

%foreign "scheme,chez:string-length"
prim__strlength : (sbj:String) -> Int

%foreign "scheme,chez:string-ref"
prim__strAt : (sbj:String) -> (idx:Int) -> Char

%foreign "scheme,chez:substring"
prim__substring : (sbj:String) -> (start:Int) -> (end:Int) -> String



-- ---------------------------------------------------------------------------
--

export
strIndexV : (i:Nat) -> (s:String)
         -> {auto 0 pr0:i < length s = True}
         -> Char
strIndexV i s = prim__strAt s (cast i)


-- ---------------------------------------------------------------------------

export strSplit : (i:Nat) -> (s:String) -> (String, String)
strSplit Z s = ("", s)
strSplit i s = let
    n = length s
    in if n <= i then (s, "")
                 else (substr 0 i s, substr i (n `minus` 1) s)

{-
export strSplitV : (i:Nat) -> (s:String)
                -> {auto 0 pr0:i <= length s}
                -> ((s0:String ** i = length s0), String)
-}


-- ---------------------------------------------------------------------------

export
strFindV : (Char -> Bool) -> (s:String)
        -> Maybe (i:Nat ** i < length s = True )
strFindV f s = go 0 (length s) Refl where
  go : (i, n:Nat) -> (0 pr1:n = length s)
    -> Maybe (i:Nat ** (i < length s = True))
  go i n pr1 with (decEq (i < n) True)
    go i n pr1 | No _ = Nothing
    go i n pr1 | Yes pr2 = let
      pr3 = rewrite sym pr1 in pr2
       in if f (strIndexV{pr0=pr3} i s)
             then Just (i ** pr3 )
             else assert_total $ go (i `plus` 1) n pr1


export
strFindrV : (Char -> Bool) -> (s:String)
        -> Maybe (i:Nat ** (i < length s = True))
strFindrV f s = go (length s) (believe_me (Refl{x=True})) where
  go : (i:Nat) -> (pr1:i <= length s = True)
    -> Maybe (i:Nat ** (i < length s = True))
  go Z _ = Nothing
  go (S i) pr1 = let
    pr3 = believe_me pr1
    pr4 = believe_me pr1
    in if f (strIndexV{pr0=pr3} i s)
         then let pr3 = believe_me pr1 in Just (i ** pr3)
         else go i pr4


-- ---------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
