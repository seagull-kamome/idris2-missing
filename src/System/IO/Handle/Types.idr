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
