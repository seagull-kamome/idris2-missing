
import Data.Container.Mutable.Array
import Data.Container.Mutable.Stack
import Data.Container.Mutable.Queue

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

  do
    putStr "IOArrayStack:"
    stk <- newIOArrayStack
    --
    chk0 !(null stk)
    --
    push 0 stk; push 1 stk
    chk0 $ not !(null stk)
    pop stk >>= chk0 . (Just 1 ==)
    pop stk >>= chk0 . (Just 0 ==)
    chk0 !(null stk)
    --
    for_ [0..5000] $ \i => push i stk
    (for [0..5000] $ \_ => pop stk) >>= chk0 . ((map Just [5000..0]) ==)

    for_ [5000..6000] $ \i => push i stk
    (for [0..1000] $ \_ => pop stk) >>= chk0 . ((map Just [6000..5000]) ==)
    putStrLn ""

  do
    putStr "IOArrayQueue:"
    q <- newIOArrayQueue
    --
    chk0 !(null q)
    --
    _ <- enqueue 0 q; _ <- enqueue 1 q
    chk0 $ not !(null q)
    dequeue q >>= chk0 . (Just 0 ==)
    dequeue q >>= chk0 . (Just 1 ==)
    chk0 !(null q)
    --
    for_ [0..5000] $ \i => enqueue i q
    (for [0..5000] $ \_ => dequeue q) >>= chk0 . ((map Just [0..5000]) ==)

    for_ [5000..6000] $ \i => enqueue i q
    (for [0..1000] $ \_ => dequeue q) >>= chk0 . ((map Just [5000..6000]) ==)
    putStrLn ""



