||| IO Handle
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module System.IO.Handle

%default total

-- ---------------------------------------------------------------------------

public export data Handle = MkHandle Int

export stdin : Handle
stdin = MkUnixHandle 0

export stdout : Handle
stdout = MkUnixHandle 1

export stderr : Handle
stderr = MkUnixHandle 2

-- ---------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
