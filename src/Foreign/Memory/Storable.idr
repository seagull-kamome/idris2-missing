||| Storable objects
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Foreign.Memory.Storable

%default total


-- --------------------------------------------------------------------------

public export
interface Storable ty where
  %inline sizeOf : Nat
  %inline alignOf : Nat
  unsafeLoad : HasIO io => (ofs:Nat) -> Ptr ty -> io ty
  unsafeStore : HasIO io => (ofs:Nat) -> ty -> Ptr ty -> io ()


-- --------------------------------------------------------------------------


%foreign "scheme,chez:foreign-ref short"
prim__foreignRef_Short : (addr:AnyPtr) -> (ofs:Int) -> PrimIO Int

%foreign "scheme,chez:foreign-ref unsigned-short"
prim__foreignRef_UShort : (addr:AnyPtr) -> (ofs:Int) -> PrimIO Int

%foreign "scheme,chez:foreign-sizeof int"
prim__foreignSizeOf_Int : Int
%foreign "scheme,chez:foreign-ref int"
prim__foreignRef_Int : (addr:Ptr Int) -> (ofs:Int) -> PrimIO Int
%foreign "scheme,chez:foreign-set int"
prim__foreignSet_Int : (addr:Ptr Int) -> (ofs:Int) -> Int -> PrimIO ()




-- %foreign "scheme,chez:foreign-ref unsigned"
-- prim__foreignRef_UInt : (addr:AnyPtr) -> (ofs:Int) -> PrimIO Int

%foreign "scheme,chez:foreign-ref size_t"
prim__foreignRef_SizeT : (addr:AnyPtr) -> (ofs:Int) -> PrimIO Int

%foreign "scheme,chez:foreign-sizeof double"
prim__foreignSizeOf_Double : Int
%foreign "scheme,chez:foreign-ref double"
prim__foreignRef_Double : (addr:Ptr Double) -> (ofs:Int) -> PrimIO Double
%foreign "scheme,chez:foreign-set double"
prim__foreignSet_Double : (addr:Ptr Double) -> (ofs:Int) -> Double -> PrimIO ()



%foreign "scheme,chez:foreign-sizeof void*"
prim__foreignSizeOf_Ptr : Int
%foreign "scheme,chez:foreign-ref void*"
prim__foreignRef_Ptr : (addr:AnyPtr) -> (ofs:Int) -> PrimIO AnyPtr
%foreign "scheme,chez:foreign-set void*"
prim__foreignSet_Ptr : (addr:AnyPtr) -> (ofs:Int) -> AnyPtr -> PrimIO ()




%foreign "scheme,chez:foreign-ref integer-8"
prim__foreignRef_I8 : (addr:Ptr Bits8) -> (ofs:Int) -> PrimIO Bits8
%foreign "scheme,chez:foreign-set integer-8"
prim__foreignSet_I8 : (addr:Ptr Bits8) -> (ofs:Int) -> Bits8 -> PrimIO ()



%foreign "scheme,chez:foreign-ref unsigned-8"
prim__foreignRef_U8 : (addr:Ptr Bits8) -> (ofs:Int) -> PrimIO Bits8
%foreign "scheme,chez:foreign-set unsigned-8"
prim__foreignSet_U8 : (addr:Ptr Bits8) -> (ofs:Int) -> Bits8 -> PrimIO ()



%foreign "scheme,chez:foreign-ref integer-16"
prim__foreignRef_I16 : (addr:Ptr Bits16) -> (ofs:Int) -> PrimIO Bits16
%foreign "scheme,chez:foreign-set integer-16"
prim__foreignSet_I16 : (addr:Ptr Bits16) -> (ofs:Int) -> Bits16 -> PrimIO ()



%foreign "scheme,chez:foreign-ref unsigned-16"
prim__foreignRef_U16 : (addr:Ptr Bits16) -> (ofs:Int) -> PrimIO Bits16
%foreign "scheme,chez:foreign-set unsigned-16"
prim__foreignSet_U16 : (addr:Ptr Bits16) -> (ofs:Int) -> Bits16 -> PrimIO ()



%foreign "scheme,chez:foreign-ref integer-132"
prim__foreignRef_I32 : (addr:Ptr Bits32) -> (ofs:Int) -> PrimIO Bits32
%foreign "scheme,chez:foreign-set integer-132"
prim__foreignSet_I32 : (addr:Ptr Bits32) -> (ofs:Int) -> Bits32 -> PrimIO ()



%foreign "scheme,chez:foreign-ref unsigned-132"
prim__foreignRef_U32 : (addr:Ptr Bits32) -> (ofs:Int) -> PrimIO Bits32
%foreign "scheme,chez:foreign-set unsigned-132"
prim__foreignSet_U32 : (addr:Ptr Bits32) -> (ofs:Int) -> Bits32 -> PrimIO ()



%foreign "scheme,chez:foreign-ref integer-164"
prim__foreignRef_I64 : (addr:Ptr Bits64) -> (ofs:Int) -> PrimIO Bits64
%foreign "scheme,chez:foreign-set integer-164"
prim__foreignSet_I64 : (addr:Ptr Bits64) -> (ofs:Int) -> Bits64 -> PrimIO ()



%foreign "scheme,chez:foreign-ref unsigned-164"
prim__foreignRef_U64 : (addr:Ptr Bits64) -> (ofs:Int) -> PrimIO Bits64
%foreign "scheme,chez:foreign-set unsigned-164"
prim__foreignSet_U64 : (addr:Ptr Bits64) -> (ofs:Int) -> Bits64 -> PrimIO ()



-- --------------------------------------------------------------------------

public export
Storable Int where
  sizeOf = fromInteger $ cast{to=Integer} prim__foreignSizeOf_Int
  alignOf = fromInteger $ cast{to=Integer} prim__foreignSizeOf_Int
  unsafeLoad o p = primIO $ prim__foreignRef_Int p (cast o)
  unsafeStore o v p = primIO $ prim__foreignSet_Int p (cast o) v

public export
Storable Double where
  sizeOf = fromInteger $ cast{to=Integer} prim__foreignSizeOf_Double
  alignOf = alignOf{ty=Int}
  unsafeLoad o p = primIO $ prim__foreignRef_Double p (cast o)
  unsafeStore o v p = primIO $ prim__foreignSet_Double p (cast o) v

public export
Storable Bits8 where
  sizeOf = 1
  alignOf = 1
  unsafeLoad o p = primIO $ prim__foreignRef_U8 p (cast o)
  unsafeStore o v p = primIO $ prim__foreignSet_U8 p (cast o) v

public export
Storable Bits16 where
  sizeOf = 2
  alignOf = 2
  unsafeLoad o p = primIO $ prim__foreignRef_U16 p (cast o)
  unsafeStore o v p = primIO $ prim__foreignSet_U16 p (cast o) v

public export
Storable Bits32 where
  sizeOf = 4
  alignOf = 4
  unsafeLoad o p = primIO $ prim__foreignRef_U32 p (cast o)
  unsafeStore o v p = primIO $ prim__foreignSet_U32 p (cast o) v

public export
Storable Bits64 where
  sizeOf = 8
  alignOf = 8
  unsafeLoad o p = primIO $ prim__foreignRef_U64 p (cast o)
  unsafeStore o v p = primIO $ prim__foreignSet_U64 p (cast o) v

public export
Storable AnyPtr where
  sizeOf = fromInteger $ cast {to=Integer} prim__foreignSizeOf_Ptr
  alignOf = fromInteger $ cast {to=Integer} prim__foreignSizeOf_Ptr
  unsafeLoad o p = primIO $ prim__foreignRef_Ptr (prim__forgetPtr p) (cast o)
  unsafeStore o v p = primIO $ prim__foreignSet_Ptr (prim__forgetPtr p) (cast o) v

public export
Storable (Ptr a) where
  sizeOf = fromInteger $ cast {to=Integer} prim__foreignSizeOf_Ptr
  alignOf = fromInteger $ cast {to=Integer} prim__foreignSizeOf_Ptr
  unsafeLoad o p =
    pure prim__castPtr <*> primIO (prim__foreignRef_Ptr (prim__forgetPtr p) (cast o))
  unsafeStore o v p =
    primIO $ prim__foreignSet_Ptr (prim__forgetPtr p) (cast o) (prim__forgetPtr v)



-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
