module Main

import Data.List.Elem
import Data.So
import Control.App
import Control.App.Console
import Test.Unit.Spec

import Data.Bitset.Classes
import Data.Bitset.Tiny
import Data.Bitset.IOBitset

%default total

testTinyBitset : Spec e => (ty:Type) ->
           (Num ty, Show ty, Eq ty,
           Set Int (TinyBitset ty), Monoid (TinyBitset ty)) => Int -> App e ()
testTinyBitset ty n = do
  describe "neutral" $ do
    tests "is zero" $ assertEqual 0 $ zero.bits
    tests "zero is empty" $ assertTrue $ all (not . flip lookup zero ) [0..n]
    tests "insert 0" $ assertEqual 1 $ (insert (the Int 0) zero).bits
    tests "insert 7" $ assertEqual 0x80 $ (insert (the Int 7) zero).bits
    tests "insert" $ assertTrue $ all (\x => lookup x (insert x zero)) [0..n]
  where
    zero : TinyBitset ty
    zero = the (TinyBitset ty) neutral


partial testIOBitset : Has [PrimIO, Spec] e => (n:Int) -> {auto _:So (n > 0)} -> App e ()
testIOBitset bssize = do
  describe "New bitset" $ do
    bs <- primIO $ newIOBitset bssize
    tests "it's empty" $ do
      for_ [0..(bssize - 1)] $ \i => do
        case choose (bssize > i && i >= 0) of
             Left proof => let (_, _) = soAnd proof
                            in (lift $ primIO $ lookup i bs) >>= assertFalse
             Right _ => pure ()

partial main : IO ()
main = run $ consoleRunSpecSimple $ do
  describe "TinyBitset" $ do
    describe "Bits8" $ testTinyBitset Bits8 7
    describe "Bits16" $ testTinyBitset Bits16 15
    describe "Bits32" $ testTinyBitset Bits32 31
    describe "Bits64" $ testTinyBitset Bits64 64
  describe "IOBitset" $ do
    for_ [100, 1024] $ \i => do
      case choose (i > 0) of
           Left _ => describe ("Size = " ++ show i) $ testIOBitset i
           Right _ => pure ()


