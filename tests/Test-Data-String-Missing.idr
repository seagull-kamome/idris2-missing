module Main

import Control.App
import Control.App.Console
import Decidable.Equality

import Test.Unit.Spec

import Data.Strings
import Data.String.Missing

testStrIndexV : Spec e => App e ()
testStrIndexV = describe "strIndexV" $ do
  tests "basic" $ do
    let str0 = "0123456789"
    let len0 = length str0
    assertEqual '0' $ strIndexV{pr0=believe_me (Refl{x=True})} 0 str0
    assertEqual '1' $ strIndexV{pr0=believe_me (Refl{x=True})} 1 str0
    assertEqual '9' $ strIndexV{pr0=believe_me (Refl{x=True})} 9 str0

testStrSplit : Spec e => App e ()
testStrSplit = describe "strSplit" $ do
  tests "basic" $ do
    let str0 = "0123456789"
    assertEqual ("012", "3456789") $ strSplit 3 str0
    assertEqual ("", "0123456789") $ strSplit 0 str0
    assertEqual ("0123456789", "") $ strSplit 10 str0
    assertEqual ("0123456789", "") $ strSplit 20 str0

testStrFindV : Spec e => App e ()
testStrFindV = describe "strFindV" $ do
  tests "basic" $ do
    let str0 = "01234567890123456789"
    assertEqual (Just 0) $ map fst $ strFindrV (== '0') str0
    assertEqual (Just 2) $ map fst $ strFindrV (== '2') str0
    assertEqual (Just 9) $ map fst $ strFindrV (== '9') str0
    assertEqual Nothing $ map fst $ strFindrV (== 'A') str0

testStrFindrV : Spec e => App e ()
testStrFindrV = describe "strFindrV" $ do
  tests "basic" $ do
    let str0 = "01234567890123456789"
    assertEqual (Just 10) $ map fst $ strFindrV (== '0') str0
    assertEqual (Just 12) $ map fst $ strFindrV (== '2') str0
    assertEqual (Just 19) $ map fst $ strFindrV (== '9') str0
    assertEqual Nothing $ map fst $ strFindrV (== 'A') str0


partial main : IO ()
main = run $ consoleRunSpecSimple $ do
  describe "Data.String.Missing" $ do
    testStrIndexV
    testStrSplit
    testStrFindV
    testStrFindrV



