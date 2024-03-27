module Randomness where

import System.Random

type Seed = Int

shuffle :: RandomGen g => Eq a => g -> [a] -> [a]
shuffle = shuffle' . randoms

shuffle' :: Eq a => [Int] -> [a] -> [a]
shuffle' (i:is) [] = []
shuffle' (i:is) xs = let (firsts, rest) = splitAt (i `mod` length xs) xs
                     in head rest : shuffle' is (filter (/= head rest) (firsts ++ tail rest))