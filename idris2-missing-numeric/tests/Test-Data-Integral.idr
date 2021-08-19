module Main

import Data.Integral.Gcd

partial main : IO ()
main = testGcd >> testLcm
  where
    testGcd : IO ()
    testGcd = do
      for_ [ (13, 0, 13), (523, 523, 0), (13, 13, 13), (1, 37, 600)
           , (20, 20, 100), (18913, 624129, 2061517) ]
           (\(ans, n, m) => do
               let x = gcd n m
               putStr $ if ans == x
                           then "✓"
                           else "✗: expect " ++ show ans
                             ++ " but actual " ++ show x
                             ++ " at gcd " ++ show n ++ " " ++ show m ++ "\n")

    testLcm : IO ()
    testLcm = do
      for_ [ (0, 0, 22), (0, 42, 0), (60, 12, 20) ]
           (\(ans, n, m) => do
               let x = lcm n m
               putStr $ if ans == x
                           then "✓"
                           else "✗: expect " ++ show ans
                             ++ " but actual " ++ show x
                             ++ " at lcm " ++ show n ++ " " ++ show m ++ "\n")


