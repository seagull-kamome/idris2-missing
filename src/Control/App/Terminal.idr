module Control.App.Terminal

import Data.Maybe
import Control.App.Console
import System.IO.Terminal as T

-- ---------------------------------------------------------------------------

export
[terminal] PrimIO e => Console e where
  putChar c = primIO $ T.putChar c
  putStr str = primIO $ T.putStr str
  getChar = primIO $ T.getChar >>= pure . fromMaybe (cast (the Int (-1)))
  getLine = primIO $ T.getLine >>= pure . fromMaybe ""


-- ---------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
