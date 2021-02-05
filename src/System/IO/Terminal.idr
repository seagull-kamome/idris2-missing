||| System console terminal
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module System.IO.Terminal

import Data.Buffer
import System.Info

import System.IO.Handle
import System.IO.Handle.Unix as U
import System.IO.Handle.Windows as W

%default total

-- ---------------------------------------------------------------------------

-- %foreign "C:GetConsoleMode,kernel32.dll"
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

export setup : HasIO io => io Bool
setup with (os)
  setup | "windows" = do -- FIXME: compile to (case (os) [("windows") ...]
    Just b <- newBuffer 4
      | Nothing => pure False
    W.MkHandle h <- W.getStdout
    primIO $ prim__win_GetConsoleMode h b
    getInt32 b 0 >>= setInt32 b 0 . prim__or_Int 0x0004
    pure True

  setup | _ = pure True


export getScreenSize : HasIO io => io (Maybe (Int, Int))
getScreenSize with (os)
  getScreenSize | "windows" = do
    Just b <- newBuffer 32
      | Nothing => pure Nothing
    W.MkHandle h <- W.getStdout
    primIO $ prim__win_GetConsoleScreenBufferInfo h b
    left   <- getBits16 b (5 * 2)
    top    <- getBits16 b (6 * 2)
    right  <- getBits16 b (7 * 2)
    bottom <- getBits16 b (8 * 2)
    pure $ Just (cast right - cast left + 1, cast bottom - cast top + 1)

  getScreenSize | "unix" = do
    Just b <- newBuffer 8
      | Nothing => pure Nothing
    primIO $ prim__ioctl_ptr 0 (0x5413 {- TIOCGWINSZ -} ) b
    r <- getBits16 b 0
    c <- getBits16 b 2
    pure $ Just (cast c, cast r)

  getScreenSize | _ = pure Nothing


-- ---------------------------------------------------------------------------

getChar : HasIO io => io (Maybe Char)
getChar = U.hGetChar U.stdin

putChar : HasIO io => Char -> io ()
putChar ch = U.hPutChar U.stdout ch >> pure ()

getLine : HasIO io => io (Maybe String)
getLine = hGetLine getChar 4096

putStr : HasIO io => String -> io ()
putStr str = U.hPutStr U.stdout str >> pure ()


putStrLn : HasIO io => String -> io ()
putStrLn str = U.hPutStrLn U.stdout str >> pure ()


-- ---------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
