||| Interface to string.h of STDC
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Foreign.Memory.String


%default total

-- --------------------------------------------------------------------------

%foreign "C,memcpy,libc 6"
export prim__memcpy : (dst:AnyPtr) -> (src:AnyPtr) -> (bytes:Int) -> PrimIO AnyPtr

%foreign "C,memmove,libc 6"
export prim__memmove : (dst:AnyPtr) -> (src:AnyPtr) -> (bytes:Int) -> PrimIO AnyPtr

%foreign "C,memccpy,libc 6"
export prim__memccpy : (dst:AnyPtr) -> (src:AnyPtr) -> (c:Int) -> (bytes:Int) -> PrimIO AnyPtr

%foreign "C,memset,libc 6"
export prim__memset : (p:AnyPtr) -> (c:Int) -> (bytes:Int) -> PrimIO AnyPtr

%foreign "C,memcmp,libc 6"
export prim__memcmp : (lhs:AnyPtr) -> (rhs:AnyPtr) -> (bytes:Int) -> PrimIO Int

%foreign "C,memchr,libc 6"
export prim__memchr : (p:AnyPtr) -> (c:Int) -> (bytes:Int) -> PrimIO AnyPtr

%foreign "C,memchrr,libc 6"
export prim__memchrr : (p:AnyPtr) -> (c:Int) -> (bytes:Int) -> PrimIO AnyPtr

%foreign "C,strcpy,libc 6"
export prim__strcpy : (dst:Ptr Char) -> (src:Ptr Char) -> PrimIO String
%foreign "C,strncpy,libc 6"
export prim__strncpy : (dst:Ptr Char) -> (src:Ptr Char) -> (n:Int) -> PrimIO String

%foreign "C,strcat,libc 6"
export prim__strcat : (dst:Ptr Char) -> (src:Ptr Char) -> PrimIO String
%foreign "C,strncat,libc 6"
export prim__strncat : (dst:Ptr Char) -> (src:Ptr Char) -> (n:Int) -> PrimIO String

-- strcmp, strncmp, strcoll, strxfrm, strcoll_l, strxfrm_l
-- strdup, strndup, strchr, strrchr, strspn, strcspn, strpbrk
-- strstr, strtok, strtok_r, strcasestr, strlen, strerror, strerror_r, strerror_l
-- strsep, strsignal, strpcpy, strpncpy, strtry, memfrob
-- basename

-- --------------------------------------------------------------------------





-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
