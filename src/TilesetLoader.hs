module TilesetLoader where

import WaveFuncCollapse ( waveFuncCollapse, Tile, Edge )

tilesetImage :: String
tilesetImage = "resources/tilesets-Circuit/whole.png"

tilesetSize :: Int
tilesetSize = 40

tilesetNImages :: Int
tilesetNImages = 14

getGrid :: (Int, Int) -> [(Int, Int, (Int, Int, Int))]
getGrid dim = concat (enumerate2D (map (map tileMapping) reversedGrid))
  where
    reversedGrid = map reverse solvedGrid 
    solvedGrid = head (waveFuncCollapse tilesetSize tileEdges dim)

enumerate2D :: [[a]] -> [[(Int, Int, a)]]
enumerate2D m = map (\(i, xs) -> map (\(j, x) -> (i,j,x)) xs) (enumerate1D (map enumerate1D m))

enumerate1D :: [a] -> [(Int, a)]
enumerate1D [] = []
enumerate1D (x:xs) = (0,x):map (\(i, a) -> (i+1,a)) (enumerate1D xs)

tileMapping :: Tile -> (Int, Int, Int)
-- bridge
tileMapping 0 = (0,0,0)
tileMapping 1 = (0,0,1)
-- -- component
tileMapping 2 = (1,0,0)
-- -- connection
tileMapping 3 = (2,0,0)
tileMapping 4 = (2,0,1)
tileMapping 5 = (2,1,0)
tileMapping 6 = (2,1,1)
-- corner
tileMapping 7 = (3,0,0)
tileMapping 8 = (3,0,1)
tileMapping 9 = (3,1,0)
tileMapping 10 = (3,1,1)
-- dskew
tileMapping 11 = (4,0,0)
tileMapping 12 = (4,0,1)
-- skew
tileMapping 13 = (5,0,0)
tileMapping 14 = (5,0,1)
tileMapping 15 = (5,1,0)
tileMapping 16 = (5,1,1)
-- substrate
tileMapping 17 = (6,0,0)
-- t
tileMapping 18 = (7,0,0)
tileMapping 19 = (7,0,1)
tileMapping 20 = (7,1,0)
tileMapping 21 = (7,1,1)
-- track
tileMapping 22 = (8,0,0)
tileMapping 23 = (8,0,1)
-- transition
tileMapping 24 = (9,0,0)
tileMapping 25 = (9,0,1)
tileMapping 26 = (9,1,0)
tileMapping 27 = (9,1,1)
-- turn
tileMapping 28 = (10,0,0)
tileMapping 29 = (10,0,1)
tileMapping 30 = (10,1,0)
tileMapping 31 = (10,1,1)
-- viad
tileMapping 32 = (11,0,0)
tileMapping 33 = (11,0,1)
-- vias
tileMapping 34 = (12,0,0)
tileMapping 35 = (12,0,1)
tileMapping 36 = (12,1,0)
tileMapping 37 = (12,1,1)
-- track
tileMapping 38 = (13,0,0)
tileMapping 39 = (13,0,1)

tileEdges :: Tile -> [Edge]
-- bridge
tileEdges 0 = [1,2,2,1]
tileEdges 1 = [2,1,1,2]
-- component
tileEdges 2 = [3,3,3,3]
-- connection
tileEdges 3 = [1,5,5,3]
tileEdges 4 = [4,3,1,4]
tileEdges 5 = [5,1,3,5]
tileEdges 6 = [3,4,4,1]
-- corner
tileEdges 7 = [6,6,5,5]
tileEdges 8 = [6,5,6,4]
tileEdges 9 = [5,6,4,6]
tileEdges 10 = [4,4,6,6]
-- dskew
tileEdges 11 = [1,1,1,1]
tileEdges 12 = [1,1,1,1]
-- skew
tileEdges 13 = [1,1,6,6]
tileEdges 14 = [1,6,1,6]
tileEdges 15 = [6,1,6,1]
tileEdges 16 = [6,6,1,1]
-- substrate
tileEdges 17 = [6,6,6,6]
-- t
tileEdges 18 = [6,1,1,1]
tileEdges 19 = [1,1,6,1]
tileEdges 20 = [1,6,1,1]
tileEdges 21 = [1,1,1,6]
-- track
tileEdges 22 = [1,6,6,1]
tileEdges 23 = [6,1,1,6]
-- transition
tileEdges 24 = [2,6,6,1]
tileEdges 25 = [6,1,2,6]
tileEdges 26 = [6,2,1,6]
tileEdges 27 = [1,6,6,2]
-- turn
tileEdges 28 = [1,1,6,6]
tileEdges 29 = [1,6,1,6]
tileEdges 30 = [6,1,6,1]
tileEdges 31 = [6,6,1,1]
-- viad
tileEdges 32 = [6,1,1,6]
tileEdges 33 = [1,6,6,1]
-- vias
tileEdges 34 = [1,6,6,6]
tileEdges 35 = [6,6,1,6]
tileEdges 36 = [6,1,6,6]
tileEdges 37 = [6,6,6,1]
-- track
tileEdges 38 = [6,2,2,6]
tileEdges 39 = [2,6,6,2]

