||| Allocate foreign memory
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Foreign.Memory.Alloc


%default total

-- --------------------------------------------------------------------------

%foreign "scheme,chez:foreign-alloc"
         "C:malloc,libc"
prim__foreignAlloc : (bytes:Int) -> PrimIO AnyPtr

%foreign "scheme,chez:foreign-free"
prim__foreignFree : AnyPtr -> PrimIO ()

-- --------------------------------------------------------------------------

export unsafeAllocateMemoryGC : {ty:Type} -> HasIO io => (bytes:Nat) -> io (GCPtr ty)
unsafeAllocateMemoryGC bytes = do
  p <- primIO $ prim__foreignAlloc (cast bytes)
  liftIO $ onCollect (prim__castPtr p)
                     (\p' => primIO $ prim__foreignFree (prim__forgetPtr p'))

export unsafeAllocateMemory : {ty:Type} -> HasIO io => (bytes:Nat) -> io (Ptr ty)
unsafeAllocateMemory bytes = do
  p <- primIO $ prim__foreignAlloc (cast bytes)
  pure $ prim__castPtr p

export unsafeFreeMemory : HasIO io => Ptr ty -> io ()
unsafeFreeMemory p = primIO $ prim__foreignFree (prim__forgetPtr p)



-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
