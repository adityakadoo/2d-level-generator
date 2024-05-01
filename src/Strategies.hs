module Strategies where

import Randomness
import WaveFuncCollapse (Strategy, Tile, single)
import Control.Monad.Random (RandomGen, mkStdGen)
import Data.List (transpose)

randomStrat :: RandomGen p => (Tile -> Int) -> Strategy p
randomStrat weights g m = (rows1, row1, xcs, row2, rows2)
  where
    xcs = shuffle g (concatMap (\c -> replicate (weights c) c) cs)
    (row1, cs : row2)    = span ((/=minEntropy).replace.fromIntegral.length) row
    (rows1, row : rows2) = span (all ((/=minEntropy).replace.fromIntegral.length)) m
    minEntropy = minimum (map (minimum . map (replace.fromIntegral.length)) m)
    replace x
        | x > 1     = x
        | otherwise = maxBound :: Tile

firstYStrat :: RandomGen p => (Tile -> Int) -> Strategy p
firstYStrat weights g m = (rows1, row1, xcs, row2, rows2)
  where
    xcs = shuffle g (concatMap (\c -> replicate (weights c) c) cs)
    (row1, cs : row2)    = span single row
    (rows1, row : rows2) = span (all single) m

badRandomStrat :: RandomGen p => Seed -> (Tile -> Int) -> Strategy p
badRandomStrat seed weights g m = (rows1, row1, xcs, row2, rows2)
  where
    xcs = shuffle (mkStdGen seed) (concatMap (\c -> replicate (weights c) c) cs)
    (row1, cs : row2)    = span ((/=minEntropy).replace.fromIntegral.length) row
    (rows1, row : rows2) = span (all ((/=minEntropy).replace.fromIntegral.length)) m
    minEntropy = minimum (map (minimum . map (replace.fromIntegral.length)) m)
    replace x
        | x > 1     = x
        | otherwise = maxBound :: Tile

ogStrat :: RandomGen p => (Tile -> Int) -> Strategy p
ogStrat weights g m = (rows1, row1, cs, row2, rows2)
  where
    (row1, cs : row2)    = span single row
    (rows1, row : rows2) = span (all single) m

entropyStrat :: RandomGen p => (Tile -> Int) -> Strategy p
entropyStrat weights g m = (rows1, row1, xcs, row2, rows2)
  where
    xcs = shuffle g (concatMap (\c -> replicate (weights c) c) cs)
    (row1, cs : row2)    = span ((>minEntropy) . replace . entropy) row
    (rows1, row : rows2) = span (all ((>minEntropy) . replace . entropy)) m
    minEntropy = minimum (map (minimum . map (replace.entropy)) m)
    replace x
        | x > 0     = x
        | otherwise = read "Infinity"::Double
    entropy = sum.map (\x -> -(x * log (x+1e-6))).(\x -> map (/sum x) x).map (fromIntegral . weights)


enumerate2D :: [[a]] -> [[(Int, Int, a)]]
enumerate2D m = zipWith (\ i xs -> map (\ (j,x) -> (i,j,x)) xs) [0..] (map (zip [0..]) m)