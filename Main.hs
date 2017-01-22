module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import qualified Bobkonf.Hello

main :: IO ()
main = defaultMain tests

-- TESTS!

tests :: TestTree
tests = testGroup "HelloWorld" [helloWorld]

helloWorld :: TestTree
helloWorld = testCase "Bobkonf.Hello.hello" prop
  where prop = Bobkonf.Hello.hello @?= "Hello World!"
