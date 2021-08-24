
import Data.Container.Mutable.Array

import Data.So
import Data.Fin


chk0 : HasIO io => Bool -> io ()
chk0 b = putStr $ if b then "✓" else "✗"



main : IO ()
main = do
  do
    putStr "Array:"
    arr <- newIOArray 100 0
    (for [0..99] $ \i => readIOArray (fromInteger i) arr) >>= chk0 . all (Just 0 ==)
    --
    for_ [0..99] $ \i => writeIOArray (fromInteger i) i arr
    traverse (\i => readIOArray (fromInteger i) arr) [0..99]
      >>= chk0 . (== map Just [0..99])
    --
    readIOArray 100 arr >>= chk0 . (== Nothing)
    readIOArray 101 arr >>= chk0 . (== Nothing)
    putStrLn ""



