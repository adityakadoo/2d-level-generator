module TilesetLoader where

import WaveFuncCollapse (Tile)
import Text.XML.Light
import Data.Set (Set)
import qualified Data.Set as Set

data Tileset = Tileset {
    imgPath :: String,
    nImages :: Int,
    tileIdx :: [Tile],
    weights :: Tile -> Int,
    neighbors :: Tile -> [[Tile]]
}

data TileEntry = TileEntry {
    name :: String,
    symmetry :: Char,
    weight :: Int
}

data NeighborEntry = NeighborEntry {
    syms :: (Char, Char),
    idxs :: (Tile, Tile)
}

fetchTileset :: String -> IO Tileset
fetchTileset tileset =
    do
        let iP = "resources/tilesets/" ++ tileset ++ "/whole.png"

        constraints <- readFile ("resources/tilesets/" ++ tileset ++ "/constraints.xml")
        let xmlData = parseXML constraints
        let xmlTiles = concatMap (findElements $ simpleName "tile")
                (concatMap (findElements $ simpleName "tiles")
                (onlyElems xmlData))
        let tileEntries = map toTileEntry xmlTiles
        let tileIdx = concatMap getIndices (zip [0..] tileEntries)
        let tileToIdx tileName = 8 * (fst.head.filter (\(idx, x) -> name x==tileName).zip [0..]) tileEntries
        let tileToSym tileName = (symmetry.head.filter (\x -> name x==tileName)) tileEntries
        let xmlNeighbors = concatMap (findElements $ simpleName "neighbor")
                (concatMap (findElements $ simpleName "neighbors")
                (onlyElems xmlData))
        let neighborEntries = map (toNeighborEntry tileToIdx tileToSym) xmlNeighbors
        let horiNeighborSet = (Set.elems.Set.fromList) (concatMap horiNeighbors neighborEntries)
        let vertNeighborSet = (Set.elems.Set.fromList) (concatMap vertNeighbors neighborEntries)
        -- print tileIdx
        -- print (length horiNeighborSet)
        -- print (length vertNeighborSet)

        return (Tileset {
            imgPath = iP,
            nImages = length tileEntries,
            tileIdx = tileIdx,
            weights = \t -> map weight tileEntries !! (fromIntegral t `div` 8),
            neighbors = \t -> [
                (map snd.filter (\(d,_) -> d==t)) vertNeighborSet,
                (map fst.filter (\(_,r) -> r==t)) horiNeighborSet,
                (map snd.filter (\(l,_) -> l==t)) horiNeighborSet,
                (map fst.filter (\(_,u) -> u==t)) vertNeighborSet
            ]
        })
        where
            simpleName s = QName s Nothing Nothing
            toTileEntry tile = TileEntry {
                name = (justGet.findAttr (simpleName "name")) tile,
                symmetry = (head.justGet.findAttr (simpleName "symmetry")) tile,
                weight = ((read :: String -> Int).justGet.findAttr (simpleName "weight")) tile
            }
            toNeighborEntry tileToIdx tileToSym tile = NeighborEntry {
                idxs = (tileToIdx l_name+l_off, tileToIdx r_name+r_off),
                syms = (tileToSym l_name, tileToSym r_name)
            }
                where
                    (l_name, l_off) = (splitToTile.words)
                        ((justGet.findAttr (simpleName "left")) tile)
                    (r_name, r_off) = (splitToTile.words)
                        ((justGet.findAttr (simpleName "right")) tile)
                    splitToTile [a] = (a, 0)
                    splitToTile [a,b] = (a, read b)
            justGet (Just a) = a
            nSym s = case s of
                'X' -> 1
                'I' -> 2
                '/' -> 2
                'T' -> 4
                'L' -> 4
                'F' -> 8
            getIndices (idx, tileEntry) = map (+ 8*idx) [0..(nSym (symmetry tileEntry) -1)]
            rot c
                | c=='X'            = \r -> 4*(r `div` 4) + (((r `mod` 4) +1) `mod` 1)
                | c=='I' || c=='/'  = \r -> 4*(r `div` 4) + (((r `mod` 4) +1) `mod` 2)
                | c=='F'            = \r -> if (r `div` 4) `mod` 2 == 1 
                                            then 4*(r `div` 4) + (((r `mod` 4) +3) `mod` 4)
                                            else 4*(r `div` 4) + (((r `mod` 4) +1) `mod` 4)
                | otherwise         = \r -> 4*(r `div` 4) + (((r `mod` 4) +1) `mod` 4)
            flip c
                | c=='I' || c=='X'  = id
                | c=='/'            = \r -> 2*(r `div` 2) + (1 - r `mod` 2)
                | c=='T'            = \r -> if r `mod` 2 == 1 then r else
                                            4*(r `div` 4) + 2*(1 -(r `mod` 4) `div` 2)
                | c=='L'            = \r -> 4*(r `div` 4) + (3 - r `mod` 4)
                | c=='F'            = \r -> 8*(r `div` 8) + ((r `mod` 8) + 4) `mod` 8
            horiNeighbors neighborEntry = [
                    (l,r),
                    (r_rot (r_rot r), l_rot (l_rot l)),
                    (l_flip l,r_flip r),
                    (r_flip (r_rot (r_rot r)), l_flip (l_rot (l_rot l)))
                ]
                where
                    (l,r) = idxs neighborEntry
                    (l_sym, r_sym) = syms neighborEntry
                    l_rot = rot l_sym
                    r_rot = rot r_sym
                    l_flip = flip l_sym
                    r_flip = flip r_sym
            vertNeighbors neighborEntry = [
                    (l,r),
                    (r_rot (r_rot r), l_rot (l_rot l)),
                    (r_flip r, l_flip l),
                    (l_flip (l_rot (l_rot l)), r_flip (r_rot (r_rot r)))
                ]
                where
                    (l',r') = idxs neighborEntry
                    (l,r) = (l_rot l', r_rot r')
                    (l_sym, r_sym) = syms neighborEntry
                    l_rot = rot l_sym
                    r_rot = rot r_sym
                    l_flip = flip l_sym
                    r_flip = flip r_sym