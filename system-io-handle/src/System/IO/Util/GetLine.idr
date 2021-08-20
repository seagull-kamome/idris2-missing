||| GetLine
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module System.IO.Util.GetLine

import Data.Buffer

%default total

-- ---------------------------------------------------------------------------

export mkGetLine' : HasIO io
                => (getc:io (Maybe Char))
                -> (buf:Buffer) -> (loc:Int)
                -> io Int
mkGetLine' getc buf loc = do
  bs <- rawSize buf
  go (bs - loc) loc
  where
    go : Int -> Int -> io Int
    go n loc = do
      if n < 0 then pure loc else do
        Just c <- getc
          | Nothing => pure loc
        if c == '\n' then pure loc else do
          setByte buf loc $ cast c
          go (assert_smaller n (n - 1)) (loc + 1)


export mkGetLine : HasIO io
               => (getc:io (Maybe Char))
               -> (maxlen:Int)
               -> io (Maybe String)
mkGetLine getc maxlen = do
  Just b <- newBuffer maxlen
    | Nothing => pure Nothing
  newloc <- mkGetLine' getc b 0
  pure $ Just !(getString b 0 newloc)


-- ---------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
