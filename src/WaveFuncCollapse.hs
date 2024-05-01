module WaveFuncCollapse where

import Data.List
import Randomness
import System.Random
import Data.Word (Word8)

type Grid = Matrix Tile
type Matrix a = [Row a]
type Row a = [a]
type Tile = Word8
type Choices = [Tile]
type Strategy p = RandomGen p => (p->Matrix Choices->(Matrix Choices,Row Choices,Choices,Row Choices,Matrix Choices))

getFirstGrid :: [Matrix Choices] -> Grid
getFirstGrid = map (map keepFilled) . head
  where
    keepFilled [a] = a
    keepFilled _ = maxBound :: Tile

waveFuncStep :: RandomGen t => t -> (Tile -> [[Tile]]) -> Strategy t -> [Matrix Choices] -> ([Matrix Choices], t)
waveFuncStep g neighbours strat (m:ms) = searchStep g neighbours strat (prune neighbours m:ms)

initGrid :: [Tile] -> (Int, Int) -> Matrix Choices
initGrid tiles (gridWidth, gridHeight) = choices tiles blank
  where
    blank = replicate gridWidth (replicate gridHeight (maxBound :: Tile))
    solved = replicate gridWidth (replicate gridHeight 0)

waveFuncCollapse :: RandomGen p => p -> [Tile] -> (Tile -> [[Tile]]) -> Strategy p -> (Int, Int) -> [Matrix Tile]
waveFuncCollapse g tiles neighbours strat dim = solver (initGrid tiles dim)
  where
    solver = search g neighbours strat . prune neighbours

choices :: [Tile] -> Grid -> Matrix Choices
choices tiles = map (map choice)
  where
    choice v = if v == (maxBound :: Tile) then tiles else [v]

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

search :: RandomGen t => t -> (Tile -> [[Tile]]) -> Strategy t -> Matrix Choices -> [Grid]
search g neighbours strat m
              | blocked neighbours m = []
              | complete m = collapse m
              | otherwise = [grid | m1 <- expand (strat g1 m),
                              grid <- search g2 neighbours strat (prune neighbours m1)]
                          where
                            (g1,g2) = split g

searchStep :: RandomGen t => t -> (Tile->[[Tile]]) -> Strategy t -> [Matrix Choices] -> ([Matrix Choices], t)
searchStep g neighbours strat (m:ms)
    | blocked neighbours m = (ms,g)
    | complete m = (m:ms,g)
    | otherwise = (expand (strat g1 m) ++ ms,g2)
      where
        (g1,g2) = split g

expand :: (Matrix Choices, Row Choices, Choices, Row Choices, Matrix Choices) -> [Matrix Choices]
expand (rows1, row1, xcs, row2, rows2) = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- xcs]

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