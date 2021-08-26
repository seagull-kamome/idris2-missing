
import Data.Container.Mutable.Array
import Data.Container.Mutable.Stack
import Data.Container.Mutable.Queue
import Data.Container.Mutable.RingQueue
import Data.Container.Mutable.Hashtable
import Data.Container.Mutable.Interfaces
import Data.Hash

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
    _ <- push 0 q; _ <- push 1 q
    chk0 $ not !(null q)
    length q >>= chk0 . (2 ==)
    --
    pull q >>= chk0 . (Just 0 ==)
    pull q >>= chk0 . (Just 1 ==)
    chk0 !(null q)
    length q >>= chk0 . (0 ==)
    --
    for_ [0..5000] $ \i => push i q
    (for [0..5000] $ \_ => pull q) >>= chk0 . ((map Just [0..5000]) ==)

    for_ [5000..6000] $ \i => push i q
    (for [0..1000] $ \_ => pull q) >>= chk0 . ((map Just [5000..6000]) ==)
    putStrLn ""

  do
    putStr "IORingQueue:"
    q <- newIORingQueue 100
    --
    chk0 !(null q)
    --
    _ <- push 0 q; _ <- push 1 q
    chk0 $ not !(null q)
    length q >>= chk0 . (2 ==)
    --
    pull q >>= chk0 . (Just 0 ==)
    pull q >>= chk0 . (Just 1 ==)
    chk0 !(null q)
    length q >>= chk0 . (0 ==)
    --
    for_ [0..99] $ \i => push i q
    push 100 q >>= chk0 . (False ==)
    (for [0..99] $ \_ => pull q) >>= chk0 . ((map Just [0..99]) ==)
    putStrLn ""

  do
    putStr "Hash:"
    {-
    print $ saltedHash 0xdeadbeef "123"
    print $ saltedHash 0xdeadbeef "456"
    print $ saltedHash 0xdeadbeef 123
    print $ saltedHash 0xdeadbeef 456
    -}
    putStrLn ""

  do
    putStr "IOHashtable:"
    ht <- the (IO (IOHashtable String Int)) $ newIOHashtable 512
    --
    chk0 !(null ht)
    --
    _ <- insert "abc" 0 ht
    _ <- insert "def" 1 ht
    _ <- insert "ghi" 2 ht
    chk0 $ not !(null ht)
    length ht >>= chk0 . (3 ==)
    lookup "abc" ht >>= chk0 . (Just 0 ==)
    lookup "def" ht >>= chk0 . (Just 1 ==)
    lookup "ghi" ht >>= chk0 . (Just 2 ==)
    --
    _ <- insert "def" 20 ht
    length ht >>= chk0 . (3 ==)
    lookup "def" ht >>= chk0 . (Just 20 ==)
    --
    _ <- delete "abc" ht
    length ht >>= chk0 . (2 ==)
    lookup "abc" ht >>= chk0 . (Nothing ==)
    putStrLn ""



