module Main

import Control.App
import Control.App.Console

import Test.Unit.Spec
import Control.App.Fresh

%default total

data FreshA : Type where
data FreshB : Type where

partial main : IO ()
main = run $ consoleRunSpec $ do
  describe "Fresh number" $ do
    runFresh FreshA $ do
      tests "fresh" $ do
        (lift $ fresh FreshA) >>= assertEqual 1
        (lift $ fresh FreshA) >>= assertEqual 2
        (lift $ fresh FreshA) >>= assertEqual 3

