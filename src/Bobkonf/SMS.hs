module Bobkonf.SMS (encode, decode) where

import Data.Bits

chunk :: Int -> [Bool] -> [[Bool]]
chunk n xs =
  if length xs <= n then
    [xs]
  else
    take n xs : chunk n (drop n xs)

boolsToInt :: Int -> [Bool] -> Int
boolsToInt _ [] = 0
boolsToInt n (b:bs) = if b then setBit (boolsToInt (n+1) bs) n else boolsToInt (n+1) bs

encode :: [Int] -> Maybe [Int]
encode xs =
  if all (\x -> x >= 0 && x <= 127) xs && length xs > 0 then
    Just $ map (boolsToInt 0) chunked
  else
    Nothing
  where toBools x = [testBit x i | i <- [0..6]]
        bools = xs >>= toBools
        chunked = chunk 8 bools

decode :: [Int] -> Maybe [Int]
decode xs =
  if all (\x -> x >= 0 && x <= 255) xs && length xs > 0 then
    Just $ if last ints == 0 then init ints else ints
  else
    Nothing
  where toBools x = [testBit x i | i <- [0..7]]
        bools = xs >>= toBools
        chunked = chunk 7 bools
        ints = map (boolsToInt 0) chunked
