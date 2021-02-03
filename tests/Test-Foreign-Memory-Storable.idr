module Main


import Control.App
import Control.App.Console

import Test.Unit.Spec
import Foreign.Memory.Alloc
import Foreign.Memory.Storable

%default total

partial
testStorable : {ty:Type} -> (Storable ty, Show ty, Eq ty)
            => Has [PrimIO, Spec, Console] e
            => (title:String) -> (v0:ty) -> (v1:ty)
            -> App e ()
testStorable {ty=ty} title v0 v1 =
  describe ("testStorable (" ++ title ++ ")") $ do
    tests "simple load-store" $ do
      let p = unsafePerformIO $ unsafeAllocateMemory 128
      let _ = unsafePerformIO $ do
        unsafeStore 0 v0 p
        unsafeStore 64 v1 p
      assertEqual v0 $ unsafePerformIO (unsafeLoad 0 p)
      assertEqual v1 $ unsafePerformIO (unsafeLoad 64 p)

      let _ = unsafePerformIO $ do
        unsafeStore 0 v1 p
        unsafeStore 64 v0 p
      assertEqual v1 $ unsafePerformIO (unsafeLoad 0 p)
      assertEqual v0 $ unsafePerformIO (unsafeLoad 64 p)
      let _ = unsafePerformIO $ unsafeFreeMemory p
      pure ()


partial main : IO ()
main = run $ consoleRunSpecSimple $ do
  testStorable{ty=Int} "Int" 0 1

