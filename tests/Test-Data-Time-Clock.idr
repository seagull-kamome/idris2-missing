module Main

import Control.App
import Control.App.Console

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.System
import Test.Unit.Spec

%default total


partial
testSystemTime : Has [PrimIO, Spec, Console] e  => App e ()
testSystemTime = describe "SystemTIme" $ do
  t <- primIO $ getSystemTime
  putStrLn $ "systemEpochDay : " ++ show systemEpochDay
  putStrLn $ "getSystemTIme : " ++ show t
  putStrLn $ "  systemToUTCTime : " ++ show (systemToUTCTime t)
  putStrLn $ "    <-> utcToSystemTime : " ++ show (utcToSystemTime (systemToUTCTime t))
  putStrLn $ "  truncateSystemTimeLeapSecond : " ++ show (truncateSystemTimeLeapSecond t)


partial
testSystemLocalTime : Has [PrimIO, Spec, Console] e  => App e ()
testSystemLocalTime = describe "SystemLocalTIme" $ do
  t <- primIO $ getSystemLocalTime
  putStrLn $ "getSystemLocalTime : " ++ show t
 

partial main : IO ()
main = run $ consoleRunSpecSimple $ do
  describe "Data.Time.Clock" $ do
    testSystemTime
    testSystemLocalTime





