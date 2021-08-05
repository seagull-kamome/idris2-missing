module Main

import Control.App
import Control.App.Console
import Decidable.Equality

import Test.Unit.Spec

import Data.Strings
import Data.String.Missing


partial main : IO ()
main = run $ consoleRunSpecSimple $ do
  describe "Data.String.Missing" $ do
    describe "strIndexV" $ do
      tests "basic" $ do
        let str0 = "0123456789"
        let len0 = length str0
        assertEqual '0' $ strIndexV{pr0=believe_me (Refl{x=True})} 0 str0
        assertEqual '1' $ strIndexV{pr0=believe_me (Refl{x=True})} 1 str0
        assertEqual '9' $ strIndexV{pr0=believe_me (Refl{x=True})} 9 str0

  describe "strSplit" $ do
    tests "basic" $ do
      let str0 = "0123456789"
      assertEqual ("012", "3456789") $ strSplit 3 str0
      assertEqual ("", "0123456789") $ strSplit 0 str0
      assertEqual ("0123456789", "") $ strSplit 10 str0
      assertEqual ("0123456789", "") $ strSplit 20 str0

  describe "strFindV" $ do
    tests "basic" $ do
      let str0 = "01234567890123456789"
      assertEqual (Just 0) $ map fst $ strFindrV (== '0') str0
      assertEqual (Just 2) $ map fst $ strFindrV (== '2') str0
      assertEqual (Just 9) $ map fst $ strFindrV (== '9') str0
      assertEqual Nothing $ map fst $ strFindrV (== 'A') str0

  describe "strFindrV" $ do
    tests "basic" $ do
      let str0 = "01234567890123456789"
      assertEqual (Just 10) $ map fst $ strFindrV (== '0') str0
      assertEqual (Just 12) $ map fst $ strFindrV (== '2') str0
      assertEqual (Just 19) $ map fst $ strFindrV (== '9') str0
      assertEqual Nothing $ map fst $ strFindrV (== 'A') str0

  describe "isPrefixOf" $ do
    tests "basic" $ do
      assertTrue $ isPrefixOf "123" "123456"
      assertTrue $ isPrefixOf "abcde" "abcde"
      assertFalse $ isPrefixOf "123" "12456"
      assertFalse $ isPrefixOf "abcde" ""




