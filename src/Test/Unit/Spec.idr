module Test.Unit.Spec

import Control.App
import Control.App.Console
import Data.Maybe
import Data.Either
import Data.List      -- for replicate

%default total

-- ---------------------------------------------------------------------------

public export
data TestFailure : Type where
  Unexpected : Show a => (expect : a) -> (actual : a) -> TestFailure
  UnexpectedMaybe : Show a => (actual : Maybe a) -> TestFailure
  UnexpectedEither : (Show a, Show b) => (actual : Either a b) -> TestFailure
Show TestFailure where
  show (Unexpected e a) = "Expected " ++ show e ++ ", But actual is " ++ show a
  show (UnexpectedMaybe x@(Just _)) = "Expected Nothing, But actual is " ++ show x
  show (UnexpectedMaybe x@Nothing) = "Expected Just _, But actual is " ++ show x
  show (UnexpectedEither x@(Left _)) = "Expected Right _, But actual is " ++ show x
  show (UnexpectedEither x@(Right _)) = "Expected Left _, But actual is " ++ show x


public export
interface UnitTest e where
  %inline assertEqual    : (Show t, Eq t) => t -> t -> App e ()
  %inline assertNotEqual : (Show t, Eq t) => t -> t -> App e ()
  %inline assertTrue     : Bool -> App e ()
  %inline assertFalse    : Bool -> App e ()
  %inline assertJust     : Show t => Maybe t -> App e ()
  %inline assertNothing  : Show t => Maybe t -> App e ()
  %inline assertRight    : (Show a, Show b) => Either a b -> App e ()
  %inline assertLeft     : (Show a, Show b) => Either a b -> App e ()

export %inline throwUnless    : Exception TestFailure e => Bool -> TestFailure -> App e ()
throwUnless b err = if b then pure () else throw err

public export
Has [Exception TestFailure] e => UnitTest e where
  assertEqual e a = throwUnless (e == a) $ Unexpected e a
  assertNotEqual e a = throwUnless (e /= a) $ Unexpected e a
  assertTrue a = throwUnless a $ Unexpected True False
  assertFalse a = throwUnless (not a) $ Unexpected False True
  assertJust a = throwUnless (isJust a) $ UnexpectedMaybe a
  assertNothing a = throwUnless (isNothing a) $ UnexpectedMaybe a
  assertRight a = throwUnless (isRight a) $ UnexpectedEither a
  assertLeft a = throwUnless (isLeft a) $ UnexpectedEither a

export runUnitTest : App (TestFailure::e) () -> App e (Maybe TestFailure)
runUnitTest tst = handle tst (pure . const Nothing) (pure . Just)

export runIOUnitTest : Has [PrimIO, Console] e => App (TestFailure::e) () -> App e (Maybe TestFailure)
runIOUnitTest tst = handle tst (pure . const Nothing) (pure . Just)



-- ---------------------------------------------------------------------------

public export
interface Spec e where
  %inline describe : String -> App e () -> App e ()
  %inline tests  : String -> App (TestFailure::e) () -> App e ()
  %inline skip     : App e () -> App e ()


record TestStatus where
  constructor MkTestStatus
  numDescribes : Int
  numTests : Int
  numFailed : Int
  skipping : Bool
  nestlevel : Int




-- ---------------------------------------------------------------------------
-- Console runner

export
0 ConsoleTest : List Error -> Type
ConsoleTest = Has [Console, PrimIO, State TestStatus TestStatus]

public export partial
[markdown] ConsoleTest e => Spec e where
  describe sbj bdy = do
    sts <- get TestStatus
    putStrLn $ (pack $ replicate (fromInteger $ cast sts.nestlevel + 1) '#') ++ (if sts.skipping then " [SKIP] " else " ") ++ sbj
    put TestStatus $ { numDescribes $= (+ 1), nestlevel $= (+ 1) } sts
    bdy
    sts' <- get TestStatus
    put TestStatus $ { nestlevel $= (\x => x - 1) } sts'
  tests sbj bdy = do
    sts <- get TestStatus
    put TestStatus $ { numTests $= (+ 1) } sts
    putStrLn $ "    - " ++ (if sts.skipping then "[SKIP] " else "") ++ sbj
    if sts.skipping
       then pure ()
       else
         runIOUnitTest bdy >>= maybe (pure ()) (\e => do
           modify TestStatus (record { numFailed $= (+ 1) })
           putStrLn $ "TestFailed!!!  " ++ show e)
  skip tst = do
    sts <- get TestStatus
    modify TestStatus $ record { skipping = True }
    tst
    modify TestStatus $ record { skipping = sts.skipping }



export partial consoleRunSpec : Has [Console, PrimIO] e =>
                                 (Spec e => App e ())
                                 -> App e ()
consoleRunSpec tst = new (MkTestStatus 0 0 0 False 0) $ do
  putStrLn "------------------------------------------------"
  tst @{markdown}
  r <- get TestStatus
  putStrLn "------------------------------------------------"
  putStrLn "Done"
  putStrLn $ "  Total desc. : " ++ show r.numDescribes
  putStrLn $ "  Total tests. : " ++ show r.numTests
  putStrLn $ "  Total failure. : " ++ show r.numFailed



data TestAt : Type where

public export partial
[simple] Has [ConsoleTest, State TestAt (List String)] e => Spec e where
  describe sbj bdy = do
    sts <- get TestStatus
    putChar $ if sts.skipping then '*' else '#'
    put TestStatus $ { numDescribes $= (+ 1), nestlevel $= (+ 1) } sts
    modify TestAt (\xs => sbj :: xs)
    bdy
    sts' <- get TestStatus
    put TestStatus $ { nestlevel $= (\x => x - 1) } sts'
    modify TestAt (\(_::xs) => xs)
  tests sbj bdy = do
    sts <- get TestStatus
    put TestStatus $ { numTests $= (+ 1) } sts
    putChar $ if sts.skipping then '.' else '-'
    if sts.skipping
       then pure ()
       else
         runIOUnitTest bdy >>= maybe (pure ()) (\e => do
           testat <- get TestAt
           modify TestStatus (record { numFailed $= (+ 1) })
           putStrLn $ "! " ++ concat (intersperse " : " (testat ++ [show e])) )
  skip tst = do
    sts <- get TestStatus
    modify TestStatus $ record { skipping = True }
    tst
    modify TestStatus $ record { skipping = sts.skipping }



export partial consoleRunSpecSimple : Has [Console, PrimIO] e =>
                                      (Spec e => App e ())
                                      -> App e ()
consoleRunSpecSimple tst =
  new {tag=TestStatus} (MkTestStatus 0 0 0 False 0) $
    new {tag=TestAt} [] $ do
      tst @{simple}
      r <- get TestStatus
      putStrLn $ "Done. " ++ show r.numFailed ++ " / " ++ show r.numTests



-- vim: tw=80 sw=2 expandtab :

