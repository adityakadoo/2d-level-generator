module WaveFuncCollapse where

import Data.List
import Randomness

type Grid = Matrix Tile
type Matrix a = [Row a]
type Row a = [a]
type Tile = Int
type Edge = Int
type Choices = [Tile]

waveFuncCollapse :: Int -> (Tile -> [Edge]) -> (Int, Int) -> [Grid]
waveFuncCollapse nTiles edges (gridWidth, gridHeight) = solver blank
  where
    solver = search nTiles edges . prune edges . choices nTiles
    border = replicate gridHeight 17:replicate (gridWidth-2)
            (17:replicate (gridHeight-2) nTiles ++ [17])
            ++ [replicate gridHeight 17]
    blank = replicate gridWidth (replicate gridHeight nTiles)
    solved = replicate gridWidth (replicate gridHeight 2)
    one = (3:replicate (gridHeight-1) nTiles):replicate (gridWidth-1) (replicate gridHeight nTiles)

choices :: Int -> Grid -> Matrix Choices
choices nTiles = map (map choice)
  where
    choice v = if v == nTiles then
                [0..nTiles-1]
               else
                [v]

prune :: (Tile -> [Edge]) -> Matrix Choices -> Matrix Choices
prune edges = pruneBy cols 0 . pruneBy rows 1
        where pruneBy f d = f . map (reduce edges d) . f

rows :: Matrix a -> [Row a]
rows = transpose

cols :: Matrix a -> [Row a]
cols = id

reduce :: (Tile -> [Edge]) -> Int -> Row Choices -> Row Choices
reduce edges d [] = []
reduce edges d [xs] = [xs]
reduce edges d (as:bs:xss) =  ras : reduce edges d (rbs:xss)
              where
                ras = filter (\a -> bottomEdge a `elem` eb) as
                eb = map topEdge rbs
                rbs = filter (\b -> topEdge b `elem` ea) bs
                ea = map bottomEdge as
                bottomEdge = (!!(3-d)) . edges
                topEdge = (!!d) . edges

search :: Int -> (Tile -> [Edge]) -> Matrix Choices -> [Grid]
search nTiles edges m | blocked edges m = []
               | complete m = collapse m
               | otherwise = [g | m1 <- expand nTiles m,
                              g <- search nTiles edges (prune edges m1)]

expand :: Int -> Matrix Choices -> [Matrix Choices]
expand nTiles m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- shuffle cs]
  where
    (row1, cs : row2)    = span ((/=minEntropy) . length) row
    (rows1, row : rows2) = span (all ((/=minEntropy) . length)) m
    minEntropy = minimum (map (minimum . replace . map length) m)
    replace [] = []
    replace (x:xs)
        | x > 1 = x:replace xs
        | otherwise = nTiles:replace xs

blocked :: (Tile -> [Edge]) -> Matrix Choices -> Bool
blocked edges m = void m || not (safe edges m)

void :: Matrix Choices -> Bool
void = any (any null)

safe :: (Tile -> [Edge]) -> Matrix Choices -> Bool
safe edges m = all (consistent edges 1) (rows m) &&
         all (consistent edges 0) (cols m)

consistent :: (Tile -> [Edge]) -> Int -> Row Choices -> Bool
consistent edges d [] = True
consistent edges d [xs] = True
consistent edges d (as:bs:xss) = not (null (ea `intersect` eb))
                                && consistent edges d (bs:xss)
              where
                eb = map topEdge bs
                ea = map bottomEdge as
                bottomEdge = (!!(3-d)) . edges
                topEdge = (!!d) . edges

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