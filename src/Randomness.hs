module Randomness where

import System.Random

seed :: Int
seed = 16

rGen :: StdGen
rGen = mkStdGen seed

shuffle :: [a] -> [a]
shuffle = shuffle' (mkStdGen seed)

shuffle' :: StdGen -> [a] -> [a]
shuffle' = modShuffle . randoms

modShuffle :: [Int] -> [a] -> [a]
modShuffle (i:is) [] = []
modShuffle (i:is) xs = let (firsts, rest) = splitAt (i `mod` length xs) xs
                     in head rest : modShuffle is (firsts ++ tail rest)