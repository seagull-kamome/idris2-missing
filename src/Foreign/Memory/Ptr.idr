||| Safe pointer
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Foreign.Memory.Ptr

import Decidable.Equality

%default total

-- --------------------------------------------------------------------------

export isNullPtr : {a:Type} -> Ptr a -> Bool
isNullPtr p = prim__nullAnyPtr (prim__forgetPtr p) /= 0

export nullPtr : {a:Type} -> Ptr a
nullPtr = prim__castPtr (prim__getNullAnyPtr)


%foreign "C:idris2_isNull, libidris2_support"
         "javascript:lambda:x=>x===undefined||x===null?1n:0n"
export prim__nullGCAnyPtr : GCAnyPtr -> Int

export isNullGCPtr : {a:Type} -> GCPtr a -> Bool
isNullGCPtr p = prim__nullGCAnyPtr (believe_me p) /= 0

-- --------------------------------------------------------------------------

public export
isNotNullPtr : {ty:Type} -> Ptr ty -> Type
isNotNullPtr ptr = isNullPtr ptr = False

public export
isNotNullGCPtr : {ty:Type} -> GCPtr ty -> Type
isNotNullGCPtr ptr = isNullGCPtr ptr = False


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
