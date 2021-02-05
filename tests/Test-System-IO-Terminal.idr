module Main

import Control.App
import Control.App.Console
import Control.App.Terminal

import System.IO.Terminal as Terminal
import Test.Unit.Spec

%default total

partial main : IO ()
main = run $ consoleRunSpecSimple $ do
  describe "System.IO.Terminal" $ do
    primIO $ do
      Terminal.setup
      Terminal.getScreenSize >>= Terminal.putStrLn . show
      Terminal.putStr "01234567890"
      Terminal.putStrLn "ABCDEFGHIJKL"
      s <- Terminal.getLine
      Terminal.putStrLn $ "It's \"" ++ show s ++ "\""




