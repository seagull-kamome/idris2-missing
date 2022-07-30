||| Basic types of IO Handle
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module System.IO.Handle.Types

import Generics.Newtype
%default total
%language ElabReflection

-- ---------------------------------------------------------------------------

public export data Permission = Readable | Writable | Executable
%runElab derive "Permission" [Generic, Meta, DecEq, Show]

export
Eq Permission where  -- ISSUE: Why newtype-deriving won't export this?
  Readable == Readable = True
  Writable == Writable = True
  Executable == Executable = True
  _ == _ = False


public export isReadable : List Permission -> Type
isReadable xs = elem Readable xs = True

public export isWritable : List Permission -> Type
isWritable xs = elem Writable xs = True

public export isExecutable: List Permission -> Type
isExecutable xs = elem Executable xs = True

-- ---------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
