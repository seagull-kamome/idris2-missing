module Main

import Control.App
import Control.App.Console

import Data.Fixed
import Test.Unit.Spec
import Text.Format.Decimal

%default total

testDecimalFormat : Spec e => App e ()
testDecimalFormat = do
  describe "Text-Format-Decimal" $ do
    describe "Int" $ do
      tests "width" $ do
        let fmt00 = record { width = Just 4, prec = Just 0, pad = Nothing,
                      spc = ' ', point = '.',
                      minus = Just (Left "-"), plus = Nothing } defaultDecimalFormat
        assertEqual "   0" $ format fmt00 0
        assertEqual "   1" $ format fmt00 1
        assertEqual "  10" $ format fmt00 10
        assertEqual " -10" $ format fmt00 (-10)
        assertEqual "-999" $ format fmt00 (-999)
        assertEqual " 999" $ format fmt00 999
        assertEqual "1234" $ format fmt00 1234
        assertEqual "-1234" $ format fmt00 (-1234)
        assertEqual "99999" $ format fmt00 99999
      tests "no width" $ do
        let fmt00 = record { width = Nothing, prec = Just 0, pad = Nothing,
                      spc = ' ', point = '.',
                      minus = Just (Left "-"), plus = Nothing } defaultDecimalFormat
        assertEqual "0" $ format fmt00 0
        assertEqual "1" $ format fmt00 1
        assertEqual "10" $ format fmt00 10
        assertEqual "-10" $ format fmt00 (-10)
        assertEqual "-999" $ format fmt00 (-999)
        assertEqual "999" $ format fmt00 999
        assertEqual "1234" $ format fmt00 1234
        assertEqual "-1234" $ format fmt00 (-1234)
        assertEqual "99999" $ format fmt00 99999
      tests "width and plus sign" $ do
        let fmt00 = record { width = Just 4, prec = Just 0, pad = Nothing,
                      spc = ' ', point = '.',
                      minus = Just (Left "-"), plus = Just (Left "+") } defaultDecimalFormat
        assertEqual "  +0" $ format fmt00 0
        assertEqual "  +1" $ format fmt00 1
        assertEqual "+123" $ format fmt00 123
        assertEqual "+1234" $ format fmt00 1234
      tests "Tailing sign" $ do
        let fmt00 = record { width = Just 4, prec = Just 0, pad = Nothing,
                      spc = ' ', point = '.',
                      minus = Just (Right "-"), plus = Nothing } defaultDecimalFormat
        assertEqual "   0" $ format fmt00 0
        assertEqual "   1-" $ format fmt00 (-1)
        assertEqual " 123-" $ format fmt00 (-123)
        assertEqual "1234-" $ format fmt00 (-1234)
      tests "Padding" $ do
        let fmt00 = record { width = Just 4, prec = Just 0, pad = Just '0',
                      spc = ' ', point = '.',
                      minus = Just (Left "-"), plus = Nothing } defaultDecimalFormat
        assertEqual "0000" $ format fmt00 0
        assertEqual "-0001" $ format fmt00 (-1)
        assertEqual "-0123" $ format fmt00 (-123)
        assertEqual "-1234" $ format fmt00 (-1234)
      tests "No width with padding" $ do
        let fmt00 = record { width = Nothing, prec = Just 0, pad = Just '0',
                      spc = ' ', point = '.',
                      minus = Just (Left "-"), plus = Nothing } defaultDecimalFormat
        assertEqual "0" $ format fmt00 0
        assertEqual "-1" $ format fmt00 (-1)
        assertEqual "-123" $ format fmt00 (-123)
        assertEqual "-1234" $ format fmt00 (-1234)

    describe "Fixed" $ do
      tests "Width" $ do
        let fmt00 = record { width = Just 4, prec = Nothing, pad = Nothing,
                      spc = ' ', point = '.',
                      minus = Just (Left "-"), plus = Nothing } defaultDecimalFormat
        assertEqual "   0.000000000000" $ format fmt00 $ the (Fixed 12) 0
        assertEqual "   1.000000000000" $ format fmt00 $ the (Fixed 12) 1
        assertEqual " -10.000000000000" $ format fmt00 $ the (Fixed 12) (-10)




partial main : IO ()
main = run $ consoleRunSpecSimple $ do
  describe "Text-Format" $ do
    testDecimalFormat



