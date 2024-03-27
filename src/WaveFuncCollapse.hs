module WaveFuncCollapse where

import Data.List
import Randomness
import System.Random

type Grid = Matrix Tile
type Matrix a = [Row a]
type Row a = [a]
type Tile = Int
type Edge = Int
type Choices = [Tile]

maxInt :: Int
maxInt = 10000

waveFuncCollapse :: Seed -> [Tile] -> (Tile -> [[Tile]]) -> (Tile -> Int) -> (Int, Int) -> [Grid]
waveFuncCollapse seed tiles neighbours info (gridWidth, gridHeight) = solver blank
  where
    solver = search (mkStdGen seed) neighbours info . prune neighbours . choices tiles
    blank = replicate gridWidth (replicate gridHeight maxInt)
    solved = replicate gridWidth (replicate gridHeight 4)

choices :: [Tile] -> Grid -> Matrix Choices
choices tiles = map (map choice)
  where
    choice v = if v == maxInt then tiles else [v]

prune :: (Tile -> [[Tile]]) -> Matrix Choices -> Matrix Choices
prune neighbours = pruneBy cols 0 . pruneBy rows 1
        where pruneBy f d = f . map (reduce neighbours d) . f

rows :: Matrix a -> [Row a]
rows = transpose

cols :: Matrix a -> [Row a]
cols = id

reduce :: (Tile -> [[Tile]]) -> Int -> Row Choices -> Row Choices
reduce neighbours d [] = []
reduce neighbours d [xs] = [xs]
reduce neighbours d (as:bs:xss) =  ras : reduce neighbours d (rbs:xss)
              where
                ras = filter (`elem` eb) as
                eb = concatMap topEdge rbs
                rbs = filter (`elem` ea) bs
                ea = concatMap bottomEdge as
                bottomEdge = (!!(3-d)) . neighbours
                topEdge = (!!d) . neighbours

search :: RandomGen g => g -> (Tile -> [[Tile]]) -> (Tile -> Int) -> Matrix Choices -> [Grid]
search g neighbours info m
              | blocked neighbours m = []
              | complete m = collapse m
              | otherwise = [g | m1 <- expand g1 info m,
                              g <- search g2 neighbours info (prune neighbours m1)]
                          where
                            (g1,g2) = split g

searchStep :: RandomGen g => g -> (Tile -> [[Tile]]) -> (Tile -> Int) -> [Matrix Choices] -> [Matrix Choices]
searchStep g neighbours info (m:ms)
    | blocked neighbours m = ms
    | complete m = m:ms
    | otherwise = expand g1 info m ++ ms
      where
        (g1,g2) = split g

expand :: RandomGen g => g -> (Tile -> Int) -> Matrix Choices -> [Matrix Choices]
expand g info m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- xcs]
  where
    xcs = shuffle g (concatMap (\c -> replicate (info c) c) cs)
    (row1, cs : row2)    = span ((/=minEntropy) . length) row
    (rows1, row : rows2) = span (all ((/=minEntropy) . length)) m
    minEntropy = minimum (map (minimum . replace . map length) m)
    replace [] = []
    replace (x:xs)
        | x > 1 = x:replace xs
        | otherwise = maxInt:replace xs

blocked :: (Tile -> [[Tile]]) -> Matrix Choices -> Bool
blocked neighbours m = void m || not (safe neighbours m)

void :: Matrix Choices -> Bool
void = any (any null)

safe :: (Tile -> [[Tile]]) -> Matrix Choices -> Bool
safe neighbours m = all (consistent neighbours 1) (rows m) &&
         all (consistent neighbours 0) (cols m)

consistent :: (Tile -> [[Tile]]) -> Int -> Row Choices -> Bool
consistent neighbours d [] = True
consistent neighbours d [xs] = True
consistent neighbours d (as:bs:xss) = not (null (as `intersect` eb)
                                  || null (ea `intersect` bs))
                                && consistent neighbours d (bs:xss)
              where
                eb = concatMap topEdge bs
                ea = concatMap bottomEdge as
                bottomEdge = (!!(3-d)) . neighbours
                topEdge = (!!d) . neighbours

complete :: Matrix Choices -> Bool
complete = all (all single)

single :: [a] -> Bool
single [_] = True
single _ = False

collapse :: Matrix [a] -> [Matrix a]
collapse m = cp (map cp m)

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [y:ys | y<-xs, ys<-cp xss]