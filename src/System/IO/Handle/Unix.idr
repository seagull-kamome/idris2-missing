module System.IO.Handle.Unix

import Data.Buffer
import Data.Strings

import public System.IO.Handle.Types

%default total

-- ---------------------------------------------------------------------------

export
data Handle : List Permission -> Type where
  MkHandle : {permission:List Permission} -> Int -> Handle permission

export stdin : Handle [Readable]
stdin = MkHandle 0

export stdout : Handle [Writable]
stdout = MkHandle 1

export stderr : Handle [Writable]
stderr = MkHandle 2

-- ---------------------------------------------------------------------------

%foreign "C:read,libc 6"
prim__unix_read_buffer : (fd:Int) -> (dst:Buffer) -> (bytes:Int) -> PrimIO Int

%foreign "C:write,libc 6"
prim__unix_write_buffer : Int -> Buffer -> Int -> PrimIO Int
%foreign "C:write,libc 6"
prim__unix_write_string : Int -> String -> Int -> PrimIO Int

%foreign "C:ioctl,libc 6"
prim__unix_ioctl_ptr : Int -> Bits32 -> Buffer -> PrimIO Int


-- ---------------------------------------------------------------------------
-- Buffer IO

export hRead' : HasIO io => Handle ps -> {auto ok:elem Readable ps = True}
             -> Buffer -> io Int
hRead' (MkHandle fd) b = do
  n <- rawSize b
  primIO $ prim__unix_read_buffer fd b n


export hRead : HasIO io => Handle ps -> {auto ok:elem Readable ps = True}
            -> (maxbytes:Int)
            -> io (Maybe (Int, Maybe Buffer))
hRead h maxbytes = do
  Just b <- newBuffer maxbytes
    | Nothing => pure $ Nothing
  r <- hRead' h b
  if r <= 0
     then do
       freeBuffer b
       pure $ Just (r, Nothing)
     else do
       pure $ Just (r, Just b)

export hWrite : HasIO io => Handle ps -> {auto ok:elem Writable ps = True}
             -> Buffer -> (bytes:Int) -> io Int
hWrite (MkHandle fd) b bytes = primIO $ prim__unix_write_buffer fd b bytes


-- ---------------------------------------------------------------------------
-- Char IO -- ofcourse very slow.

export hPutChar : HasIO io => Handle ps -> {auto ok:elem Writable ps = True}
               -> Char -> io Int
hPutChar (MkHandle fd) ch = do
  primIO $ prim__unix_write_string fd (singleton ch) 1

export hGetChar' : HasIO io => Handle ps -> {auto ok:elem Readable ps = True}
                -> io (Maybe (Either Int Char))
hGetChar' h = do
  Just (n, Just b) <- hRead h 1
    | Nothing => pure Nothing
    | Just (n, Nothing) => pure $ Just $ Left n
  ch <- getByte b 0
  pure $ Just $ Right $ cast{to=Char} ch


export hGetChar : HasIO io => Handle ps -> {auto ok:elem Readable ps = True}
               -> io (Maybe Char)
hGetChar h = do
  Just (Right ch) <- hGetChar' h
    | Nothing => pure Nothing
    | Just (Left _) => pure Nothing
  pure $ Just ch


-- ---------------------------------------------------------------------------
-- String IO


export hPutStr : HasIO io => Handle ps -> {auto ok:elem Writable ps = True}
              -> String -> io Int
hPutStr (MkHandle fd) str = do
  let n = cast $ length str
  primIO $ prim__unix_write_string fd str n

export hPutStrLn : HasIO io => Handle ps -> {auto ok:elem Writable ps = True}
               -> String -> io Int
hPutStrLn h str = hPutStr h $ str ++ "\n"




-- ---------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
