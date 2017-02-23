{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Data.List as List
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Test.QuickCheck.Poly
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import qualified Bobkonf.Hello   as BEx0
import qualified Bobkonf.Sorting as BEx1
import qualified Bobkonf.SMS     as BEx2

main :: IO ()
main = defaultMain tests

-- TESTS!

tests :: TestTree
tests = testGroup "Bobkonf"
  [ exercise0,
    exercise1,
    exercise2
  ]

-- Exercise 0: Hello World
--
-- `BEx0` exports a function `hello` which returns the string "Hello World!".
-- Test this.

exercise0 :: TestTree
exercise0 = QC.testProperty "Hello World" prop
  where prop = BEx0.hello == "Hello World!"

-- Exercise 1: Sorting
--
-- A sorting function is a function that may take an arbitrary list of an
-- arbitrary type `a` (as long as it satisfied the `Ord` constraint; that is,
-- its elements can be compared for less/equal/greater than) and sorts it.
--
-- `BEx1` exports three functions `sort1`, `sort2` and `sort3`. All of them
-- claim to be "sorting" functions. They're all wrong in some subtle, but
-- different ways. Write the tests to uncover their flaws.
--
-- Make sure that your tests do not reject `Data.List.sort` for it is a valid
-- sorting function.
--
-- Add your properties below to `sortingTests` as `prop1`, `prop2`, ...
-- Exercise is completed when
--   * for each sorting function, at least one property fails, and
--   * for `Data.List.sort`, all properties succeed

type SortingFunction = forall a. Ord a => [a] -> [a]

exercise1 :: TestTree
exercise1 = testGroup "Sorting"
  [ sortingTests "sort1" BEx1.sort1
  , sortingTests "sort2" BEx1.sort2
  , sortingTests "sort3" BEx1.sort3
  , sortingTests "reference" List.sort
  ]

sortingTests :: TestName -> SortingFunction -> TestTree
sortingTests name sort = testGroup name [propDummy, prop1, prop2, prop3]
  where propDummy = QC.testProperty "Dummy property" $ \xs ->
                      sort xs == sort (xs::[OrdA])
        prop1 = QC.testProperty "Preserves elements" $ \xs ->
                  Set.fromList (sort xs) == Set.fromList (xs::[OrdA])
        prop2 = QC.testProperty "Perserves duplicates" $ \xs ->
                  List.all (\x -> length (filter (x ==) xs) == length (filter (x ==) (sort xs))) (xs::[OrdA])
        prop3 = QC.testProperty "Stability" $ \xs ->
                  let asc ((Indexed x1 n1) : (Indexed x2 n2) : xs) = (x1 /= x2 || n1 < n2) && asc (Indexed x2 n2 : xs)
                      asc _ = True
                  in asc (sort (zipWith Indexed (xs::[OrdA]) [0..]))

data Indexed a = Indexed a Int
instance Eq a => Eq (Indexed a) where
  (Indexed x1 _) == (Indexed x2 _) = x1 == x2
instance Ord a => Ord (Indexed a) where
  (Indexed x1 _) < (Indexed x2 _) = x1 < x2
  (Indexed x1 _) <= (Indexed x2 _) = x1 <= x2

-- Exercise 2: SMS encoding/decoding
--
-- This example is based on a demonstration of Erlang QuickCheck by John
-- Hughes.
--
-- Text messages (SMS) use 7-bit characters. The transport medium supports
-- 8-bit characters. This gives an opportunity for compression: We can pack
-- 8 SMS characters into 7 transport characters, by just concatenating the
-- bits.
--
-- Bit no.    0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15  ...     55
-- Byte no.  |          0            |           1          | ...   7   |
-- Character |        0           |          1        |       ...   8   |
-- Bit no.    0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15  ...     55
--
-- `BEx2` exports a pair of functions, `encode` and `decode`.
--
-- `encode` takes a list of integers that represent SMS characters and returns
-- a list of integers that represent transport characters.
--
-- `decode` is exactly the opposite.
--
-- Naturally, both `encode` and `decode` have a set of preconditions as to what
-- are wellformed inputs.
--
-- You know nothing about the `encode` and `decode` functions, but you are
-- allowed to experiment with them in the REPL.
--
-- Complete the following tasks:
--   * figure out the preconditions
--   * check that encoding produces a wellformed encoding
--   * check that decoding consumes a wellformed encoding
--   * check that decoding an encoded result yields the input
--   * find the error in the implementation and describe how to fix the
--     precondition to account for it

exercise2 :: TestTree
exercise2 = localOption (QC.QuickCheckMaxRatio 100) $ localOption (QC.QuickCheckTests 1000) $ testGroup "SMS"
  [ propEncodePrecondDecode
  , propEncodePrecondJust
  , propDecodePrecondJust
  , localOption (QC.QuickCheckMaxSize 10) $ propDecodeEncode
  ]
  where propEncodePrecondDecode =
          -- This should be valid.
          QC.testProperty "Encode produces decodable output" $ \xs ->
            case BEx2.encode xs of
              Just ys -> decodePrecondition ys
              Nothing -> True
        propEncodePrecondJust =
          -- This should be valid.
          QC.testProperty "Encode produces Just for encodable input" $ \xs ->
            encodePrecondition xs ==>
              isJust (BEx2.encode xs)
        propDecodePrecondJust =
          -- This should be valid.
          QC.testProperty "Decode produces Just for decodable input" $ \xs ->
            decodePrecondition xs ==>
              isJust (BEx2.decode xs)
        propDecodeEncode =
          -- This should be invalid.
          QC.testProperty "Decode after encode returns the initial input" $ \xs ->
            encodePrecondition xs ==>
               failurePrecondition xs ==>
               -- uncomment this and implement `failurePrecondition` to make it
               -- more likely for QuickCheck to find a counterexample
                (BEx2.encode xs >>= BEx2.decode) == Just xs
           -- After you made this test fail, change the precondition to make it
           -- succeed (and keep the other tests green).

encodePrecondition :: [Int] -> Bool
encodePrecondition xs = all (\x -> x >= 0 && x <= 127) xs && length xs > 0

decodePrecondition :: [Int] -> Bool
decodePrecondition xs = all (\x -> x >= 0 && x <= 255) xs && length xs > 0

failurePrecondition :: [Int] -> Bool
failurePrecondition xs = last xs == 0
