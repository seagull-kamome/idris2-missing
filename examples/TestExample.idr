module Main

import Control.App
import Control.App.Console
import Test.Unit.Spec

main : IO ()
main = run $ consoleTestRunner $ do
  describe "test0" $ do
    describe "test0-1" $ do
      tests "aaaaa" $ do
        pure 1 >>= assertEqual 1
      tests "bbb" $ do
        pure True >>= assertTrue
    describe "test0-2" $ do
      tests "aaaaa" $ do
        pure 1 >>= assertEqual 1
      tests "bbb" $ do
        pure True >>= assertTrue
    describe "Boolean" $ do
      describe "Expecting True" $ do
        tests "it is" $ pure True >>= assertTrue
        tests "it isn't" $ pure False >>= assertTrue
      describe "Expecting False" $ do
        tests "it isn't" $ pure True >>= assertFalse
        tests "it is" $ pure False >>= assertFalse
    describe "Maybe" $ do
      describe "Expecting Nothing" $ do
        tests "it is" $ do
          pure (the (Maybe Int) Nothing) >>= assertNothing
        tests "it isn't" $ do
          pure (Just 2) >>= assertNothing
      describe "Expecting Just" $ do
        tests "it isn't" $ do
          pure (the (Maybe Int) Nothing) >>= assertJust
        tests "it is" $ do
          pure (Just 2) >>= assertJust



