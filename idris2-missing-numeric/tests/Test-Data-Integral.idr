module Main

import Data.Fixed
import Data.Integral.Gcd
import Data.Complex
import Text.Format.Decimal

chk : (Show a, Show b) => String -> List (a, b) -> (b -> a) -> (b -> a -> a -> Bool) -> IO ()
chk dsc xs f g = do
  putStr (dsc ++ ": ")
  for_ xs $
    \xs@(e, x) => do
      let ans = f x
      if g x e ans
         then putStr "✓"
         else putStrLn ("\n  ✗ test=" ++ show xs ++ " ans=" ++ show ans)
  putStrLn ""


partial main : IO ()
main = testGcd >> testLcm >> testTextFormat >> testComplex
  where
    testGcd : IO ()
    testGcd = chk "Gcd golden"
          [ (13, (0, 13)), (523, (523, 0)), (13, (13, 13)), (1, (37, 600))
          , (20, (20, 100)), (18913, (624129, 2061517)) ]
          (uncurry gcd) (const (==))

    testLcm : IO ()
    testLcm = chk "Lcm golden"
          [ (0, 0, 22), (0, 42, 0), (60, 12, 20) ]
          (uncurry lcm) (const (==))

    testTextFormat : IO ()
    testTextFormat = do
      do
        let fmt00 = record { width = Just 4, prec = Just 0, pad = Nothing,
                           spc = ' ', point = '.',
                           minus = Just (Left "-"), plus = Nothing } defaultDecimalFormat
        chk "Text format (Int + Width)"
            [ ("   0", 0), ("   1", 1), ("  10", 10), (" -10", -10)
            , ("-999", -999), (" 999", 999), ("1234", 1234), ("-1234", -1234)
            , ("99999", 99999) ]
            (format fmt00) (const (==))
      do
        let fmt00 = record { width = Nothing, prec = Just 0, pad = Nothing,
                      spc = ' ', point = '.',
                      minus = Just (Left "-"), plus = Nothing } defaultDecimalFormat
        chk "Text format (Int + no width)"
            [ ("0", 0), ("1", 1), ("10", 10), ("-10", -10), ("-999", -999)
            , ("999", 999), ("1234", 1234), ("-1234", -1234), ("99999", 99999) ]
            (format fmt00) (const (==))
      do
        let fmt00 = record { width = Just 4, prec = Just 0, pad = Nothing,
                      spc = ' ', point = '.',
                      minus = Just (Left "-"), plus = Just (Left "+") } defaultDecimalFormat
        chk "Text format (Int + Width and PlusSign"
            [ ("  +0", 0), ("  +1", 1), ("+123", 123), ("+1234", 1234) ]
            (format fmt00) (const (==))

      do
        let fmt00 = record { width = Just 4, prec = Just 0, pad = Nothing,
                      spc = ' ', point = '.',
                      minus = Just (Right "-"), plus = Nothing } defaultDecimalFormat
        chk "Text format (Int + TailingSign)"
            [ ("   0", 0), ("   1-", -1), (" 123-", -123), ("1234-", -1234) ]
            (format fmt00) (const (==))

      do
        let fmt00 = record { width = Just 4, prec = Just 0, pad = Just '0',
                      spc = ' ', point = '.',
                      minus = Just (Left "-"), plus = Nothing } defaultDecimalFormat
        chk "Text format (Int + Padding)"
            [ ("0000", 0), ("-0001", -1), ("-0123", -123), ("-1234", -1234) ]
            (format fmt00) (const (==))

      do
        let fmt00 = record { width = Nothing, prec = Just 0, pad = Just '0',
                      spc = ' ', point = '.',
                      minus = Just (Left "-"), plus = Nothing } defaultDecimalFormat
        chk "Text format (Int + no width + padding)"
            [ ("0", 0), ("-1", -1), ("-123", -123), ("-1234", -1234) ]
            (format fmt00) (const (==))

      do
        let fmt00 = record { width = Just 4, prec = Nothing, pad = Nothing,
                      spc = ' ', point = '.',
                      minus = Just (Left "-"), plus = Nothing } defaultDecimalFormat
        chk "Text format (Fixed + Width)"
            [("   0.000000000000", the (Fixed 12) 0),
             ("   1.000000000000", 1), (" -10.000000000000", -10) ]
            (format fmt00) (const (==))

    testComplex : IO ()
    testComplex = do
      chk "Complex Eq"
          [ (True, (MkComplex 0 1, MkComplex 0 1))
          , (False, (MkComplex 1 1, MkComplex 0 1)) ]
        (uncurry (==)) (const (==))

      


