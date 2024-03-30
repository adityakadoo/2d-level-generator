module TilesetLoader where

import WaveFuncCollapse
import Randomness
import TSCastle
import Graphics.Rendering.OpenGL (GLfloat)
import Data.List (transpose)
import Control.Monad.Random
import Data.Word (Word8)

getTilesetImage :: String
getTilesetImage = textureImg

getNImages :: Int
getNImages = nImages

getTilesetSize :: Int
getTilesetSize = length tileIdx

getGrid :: RandomGen g => g -> (Int, Int) -> [[(Tile, Word8)]]
getGrid g dim = map (map (, 1)) (transpose solvedGrid)
  where
    solvedGrid = head (waveFuncCollapse g tileIdx tileNeighbours tileInfo dim)

stepChoices :: RandomGen g => g -> [Matrix Choices] -> ([Matrix Choices], g)
stepChoices g = waveFuncStep g tileNeighbours tileInfo

initChoices :: (Int, Int) -> [Matrix Choices]
initChoices dim = [initGrid tileIdx dim]

getFirstGrid :: [Matrix Choices] -> [[(Tile, Word8)]]
getFirstGrid = transpose . map (map (\cs -> (head cs, fromIntegral (length cs)))) . head

enumerate2D :: [[a]] -> [[(Int, Int, a)]]
enumerate2D m = map (\(i, xs) -> map (\(j, x) -> (i,j,x)) xs) (enumerate1D (map enumerate1D m))

enumerate1D :: [a] -> [(Int, a)]
enumerate1D [] = []
enumerate1D (x:xs) = (0,x):map (\(i, a) -> (i+1,a)) (enumerate1D xs)

tileMapping :: (Tile, Word8) -> (Int, (Int, Int, Int), Int)
tileMapping (tile,entropy) = (t1,
  (((r `div` 2) + (r `mod` 2)) `mod` 2,
  r `div` 2, z), fromIntegral entropy)
  where
    t1 = fromIntegral tile `div` 8
    r = fromIntegral tile `mod` 4
    z = fromIntegral (tile `mod` 8) `div` 4