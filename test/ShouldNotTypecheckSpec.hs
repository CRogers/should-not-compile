{-# LANGUAGE GADTs, TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

import Control.DeepSeq
import Control.Exception
import GHC.Generics (Generic)
import Test.Hspec
import Test.Hspec.Expectations (expectationFailure)
import qualified Test.HUnit.Lang as HL
import Test.ShouldNotTypecheck

data Result
  = Success
  | Failure
  | Error String

#if MIN_VERSION_HUnit(1,3,0)
toResult :: HL.Result -> Result
toResult result = case result of
  HL.Success -> Success
  HL.Failure _ _ -> Failure
  HL.Error _ msg -> Error msg
#else
toResult :: Maybe (Bool, String) -> Result
toResult result = case result of
  Nothing -> Success
  Just (True, _) -> Failure
  Just (False, msg) -> Error msg
#endif

shouldFailAssertion :: IO () -> IO ()
shouldFailAssertion value = do
  result <- HL.performTestCase value
  case toResult result of
    Success   -> expectationFailure "Did not throw an assertion error"
    Failure   -> return ()
    Error msg -> expectationFailure $ "Raised an error " ++ msg

shouldThrowException :: Exception e => e -> IO () -> IO ()
shouldThrowException exception value = do
  result <- HL.performTestCase value
  case toResult result of
    Success   -> expectationFailure "Did not throw exception: assertion succeeded"
    Failure   -> expectationFailure "Did not throw exception: assertion failed"
    Error msg -> case msg == show exception of
      True -> return ()
      False -> expectationFailure "Incorrect exception propagated"

data Expr t where
  IntVal :: Int -> Expr Int
  BoolVal :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int

instance NFData (Expr t) where
  rnf expr = case expr of
    IntVal i -> rnf i
    BoolVal b -> rnf b
    Add l r -> rnf l `seq` rnf r

data NoNFDataInstance = NoNFDataInstance

main :: IO ()
main = hspec $ do
  describe "shouldNotTypecheck" $ do
    it "should not throw an assertion error when an expression is ill typed" $ do
      shouldNotTypecheck ("foo" :: Int)

    it "should throw an assertion error when an expression is well typed" $ do
      shouldFailAssertion (shouldNotTypecheck ("foo" :: String))

    it "should throw an actual exception and not fail the assertion if the expression contains an non-HUnitFailure exception" $ do
      let exception = NoMethodError "lol"
      shouldThrowException exception (shouldNotTypecheck (throw exception :: Int))

    it "should propagate an actual exception and not fail the assertion if the expression contains a non-deferred ErrorCall exception" $ do
      let exception = ErrorCall "yay"
      shouldThrowException exception (shouldNotTypecheck (throw exception :: Int))

    it "should not throw an assertion when an expression with more than one level of constructors is ill typed" $ do
      shouldNotTypecheck (Add (BoolVal True) (IntVal 4))

    it "should warn if an expression had a type error due to lack of NFData instance" $ do
      shouldFailAssertion (shouldNotTypecheck NoNFDataInstance)


  describe "shouldNotTypecheckWith" $ do
     it "passes if the given string can be found in the error message" $ do
       shouldNotTypecheckWith "Couldn't match expected type ‘Bool’ with actual type ‘()’"
         (not ())

     it "throws an exception if the given string is not found in the error message" $ do
       shouldFailAssertion $
         shouldNotTypecheckWith "foo"
           (not ())

