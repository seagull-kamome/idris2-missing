||| Basic types of IO Handle
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module System.IO.Handle.Types


%default total


-- ---------------------------------------------------------------------------

public export data Permission = Readable | Writable
public export
Eq Permission where
  Readable == Readable = True
  Writable == Writable = True
  _ == _ = False


-- ---------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
