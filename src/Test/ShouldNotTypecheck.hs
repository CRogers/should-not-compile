{-# LANGUAGE CPP, RankNTypes, GADTs #-}
#if __GLASGOW_HASKELL__ >= 800
#define TheExc TypeError
#else
#define TheExc ErrorCall
#endif
module Test.ShouldNotTypecheck (shouldNotTypecheck, shouldNotTypecheckWith) where

import Data.Char
import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate, try, throwIO, TheExc(..))
import Data.List (isSuffixOf, isInfixOf, isPrefixOf)
import Test.HUnit.Lang (Assertion, assertFailure)

{-|
  Takes one argument, an expression that should not typecheck.
  It will fail the test if the expression does typecheck.
  Requires Deferred Type Errors to be enabled for the file it is called in.
  See the <https://github.com/CRogers/should-not-typecheck#should-not-typecheck- README>
  for examples and more information.
-}
#if __GLASGOW_HASKELL__ >= 800
shouldNotTypecheck :: NFData a => (() ~ () => a) -> Assertion
#else
shouldNotTypecheck :: NFData a => a -> Assertion
#endif
shouldNotTypecheck = shouldNotTypecheckWith ""

{-|
   Like 'shouldNotTypecheck', but ensures that the given substring appears in
   the reuslting type error.
-}
#if __GLASGOW_HASKELL__ >= 800
shouldNotTypecheckWith :: NFData a => String -> (() ~ () => a) -> Assertion
#else
shouldNotTypecheckWith :: NFData a => String -> a -> Assertion
#endif
-- The type for GHC-8.0.1 is a hack, see https://github.com/CRogers/should-not-typecheck/pull/6#issuecomment-211520177
shouldNotTypecheckWith substring a = do
  result <- try (evaluate $ force a)
  case result of
    Right _ -> assertFailure "Expected expression to not compile but it did compile"
    Left e@(TheExc msg) -> case isSuffixOf "(deferred type error)" msg of
      True -> case isInfixOf "No instance for" msg && isInfixOf "NFData" msg of
        True -> assertFailure $ "Make sure the expression has an NFData instance! See docs at https://github.com/CRogers/should-not-typecheck#nfdata-a-constraint. Full error:\n" ++ msg
        False -> case isInfixOf substring $ stripContext msg of
           True -> return ()
           False -> assertFailure ("type error did not contain \"" ++ substring ++ "\":\n" ++ msg)
      False -> throwIO e


{-|
  In some versions of GHC, type errors contain a context which describes where
  the error message was thrown from. This will include the call to
  'shouldNotTypecheckWith', which unless handled, would cause
  'shouldNotTypecheckWith' to always succeed.  This function strips off the
  context from the error messages.
-}
stripContext :: String -> String
stripContext = unlines
             . takeWhile (not . isPrefixOf "In the " . dropWhile (\x -> isSpace x || x == '•'))
             . lines

