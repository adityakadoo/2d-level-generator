module TilesetLoader where

import WaveFuncCollapse ( Edge, Tile, waveFuncCollapse )
import Randomness ( Seed )
import TSCircuit
import Graphics.Rendering.OpenGL (GLfloat)
import Data.List (transpose)

getTilesetImage :: String
getTilesetImage = textureImg

getNImages :: Int
getNImages = nImages

getGrid :: Int -> (Int, Int) -> [[Tile]]
getGrid seed dim = transpose solvedGrid
  where
    solvedGrid = head (waveFuncCollapse seed tileIdx tileNeighbours tileInfo dim)

enumerate2D :: [[a]] -> [[(Int, Int, a)]]
enumerate2D m = map (\(i, xs) -> map (\(j, x) -> (i,j,x)) xs) (enumerate1D (map enumerate1D m))

enumerate1D :: [a] -> [(Int, a)]
enumerate1D [] = []
enumerate1D (x:xs) = (0,x):map (\(i, a) -> (i+1,a)) (enumerate1D xs)

tileMapping :: Tile -> (Int, Int, Int, Int)
tileMapping tile = (t1,
  ((r `div` 2) + (r `mod` 2)) `mod` 2,
  r `div` 2, z)
  where
    t1 = tile `div` 8
    r = tile `mod` 4
    z = (tile `mod` 8) `div` 4