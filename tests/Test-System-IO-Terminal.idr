module Main

import Control.App
import Control.App.Console

import System.IO.Terminal as Terminal
import Test.Unit.Spec

%default total

partial main : IO ()
main = run $ consoleRunSpecSimple $ do
  describe "Data.Time.Clock" $ do
    primIO $ do
      Terminal.setup
      Terminal.getScreenSize >>= putStrLn . show






