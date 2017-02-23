module Bobkonf.Sorting (sort1, sort2, sort3) where

import Data.List (sort)

-- discards all elements
sort1 :: [a] -> [a]
sort1 _ = []

-- meddles with duplicate elements
sort2 :: Ord a => [a] -> [a]
sort2 = frobnicate . sort
  where frobnicate (x1 : x2 : x3 : xs) = if x1 == x2 then x1 : x3 : x3 : frobnicate xs else x1 : frobnicate (x2 : x3 : xs)
        frobnicate xs = xs

-- unstable
sort3 :: Ord a => [a] -> [a]
sort3 = shuffle . sort
  where shuffle (x1 : x2 : xs) = if x1 == x2 then x2 : x1 : shuffle xs else x1 : shuffle (x2 : xs)
        shuffle xs = xs
