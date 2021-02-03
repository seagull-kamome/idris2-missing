module System.IO.Handle.Types

import System.Info

%default total


-- ---------------------------------------------------------------------------

public export data SystemHandleRep = WindowsHandle | UnixHandle
public export
data Handle' : SystemHandleRep -> Type where
  MkWindowsHandle : (h:AnyPtr) -> Handle' WindowsHandle
  MkUnixHandle : (h:Int) -> Handle' UnixHandle

export Handle : Type
Handle with (os)
  Handle | "windows" = Handle' WindowsHandle
  Handle | "unix"    = Handle' UnixHandle
  Handle | _         = ?unknown_runtime_system


-- ---------------------------------------------------------------------------

-- %foreign "scheme,chez:(foreign-procedure __stdcall \"GetStdHandle\" (unsigned-32) void*)"
%foreign "scheme,chez:(foreign-procedure #f \"GetStdHandle\" (unsigned-32) void*)"
prim__win_GetStdHandle : Bits32 -> AnyPtr

-- ---------------------------------------------------------------------------

export stdin : {rep:SystemHandleRep} -> Handle' rep
stdin {rep=WindowsHandle} = MkWindowsHandle $ prim__win_GetStdHandle $ cast (-10)
stdin {rep=UnixHandle} = MkUnixHandle 0

export stdout : {rep:SystemHandleRep} -> Handle' rep
stdout {rep=WindowsHandle} = MkWindowsHandle $ prim__win_GetStdHandle $ cast (-11)
stdout {rep=UnixHandle} = MkUnixHandle 1

export stderr : {rep:SystemHandleRep} -> Handle' rep
stderr {rep=WindowsHandle} = MkWindowsHandle $ prim__win_GetStdHandle $ cast (-12)
stderr {rep=UnixHandle} = MkUnixHandle 2


-- ---------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
