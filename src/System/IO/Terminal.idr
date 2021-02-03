||| System console terminal
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module System.IO.Terminal

import Data.Buffer
import System.Info

import System.IO.Handle

%default total

-- ---------------------------------------------------------------------------

-- %foreign "scheme,chez:(foreign-procedure __stdcall \"GetConsoleMode\" (void* u32*) int)"
%foreign "scheme,chez:(foreign-procedure #f \"GetConsoleMode\" (void* u32*) int)"
prim__win_GetConsoleMode : AnyPtr -> Buffer -> PrimIO Int

%foreign "scheme,chez:(foreign-procedure #f \"GetConsoleMode\" (void* unsigned-32) int)"
prim__win_SetConsoleMode : AnyPtr -> (mode:Int) -> PrimIO Int

%foreign "scheme,chez:(foreign-procedure #f \"GetConsoleScreenBufferInfo\" (void* void*) int)"
prim__win_GetConsoleScreenBufferInfo : AnyPtr -> Buffer -> PrimIO Int

-- ---------------------------------------------------------------------------

%foreign "C:ioctl,libc 6"
prim__ioctl_ptr : Int -> Bits32 -> Buffer -> PrimIO Int

-- ---------------------------------------------------------------------------

export partial setup : HasIO io => io ()
setup with (os)
  setup | "windows" = do
    Just b <- newBuffer 4
    let (MkWindowsHandle h) = stdout{rep=WindowsHandle}
    primIO $ prim__win_GetConsoleMode h b
    getInt32 b 0 >>= setInt32 b 0 . prim__or_Int 0x0004
    pure ()
  setup | _ = pure ()


export partial getScreenSize : HasIO io => io (Int, Int)
getScreenSize with (o')
  getScreenSize | "windows" = do
    Just b <- newBuffer 32
    let (MkWindowsHandle h) = stdout{rep=WindowsHandle}
    primIO $ prim__win_GetConsoleScreenBufferInfo h b
    left   <- getBits16 b (5 * 2)
    top    <- getBits16 b (6 * 2)
    right  <- getBits16 b (7 * 2)
    bottom <- getBits16 b (8 * 2)
    pure (cast right - cast left + 1, cast bottom - cast top + 1)
  getScreenSize | "unix" = do
    Just b <- newBuffer 8
    primIO $ prim__ioctl_ptr 0 (0x5413 {- TIOCGWINSZ -} ) b
    r <- getBits16 b 0
    c <- getBits16 b 2
    pure $ (cast c, cast r)
  getScreenSize | _ = pure (0, 0)

-- ---------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
