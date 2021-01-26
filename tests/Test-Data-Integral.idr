module Main

import Control.App
import Control.App.Console

import Test.Unit.Spec
import Data.Integral.Gcd

partial main : IO ()
main = run $ consoleRunSpecSimple $ do
  describe "Data-Integral" $ do
    describe "Integral" $ do
      tests "GCD" $ do
        assertEqual 13 $ gcd 0 13
        assertEqual 523$ gcd 523 0
        assertEqual 13 $ gcd 13 13
        assertEqual 1 $ gcd 37 600
        assertEqual 20 $ gcd 20 100
        assertEqual 18913 $ gcd 624129 2061517

      tests "LCM" $ do
        assertEqual 0 $ lcm 0 22
        assertEqual 0 $ lcm 42 0
        assertEqual 60 $ lcm 12 20



