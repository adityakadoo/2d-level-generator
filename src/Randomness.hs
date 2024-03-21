module Randomness where

import System.Random

type Seed = Int

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle = modShuffle . randoms

modShuffle :: [Int] -> [a] -> [a]
modShuffle (i:is) [] = []
modShuffle (i:is) xs = let (firsts, rest) = splitAt (i `mod` length xs) xs
                     in head rest : modShuffle is (firsts ++ tail rest)