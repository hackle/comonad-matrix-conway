{-# Language DeriveFunctor #-}

module Game where

import Data.Bifunctor
import Control.Comonad
import Data.Bool
import Data.Matrix

type Coord = (Int, Int)
data Game a = Game { 
    coords :: Matrix a
    , focus :: Coord 
    } deriving (Functor, Show)

-- the trick of contextual computation with Comonads is all in 
-- duplicate + extend
-- duplicate distributes the focus 
-- and extend acts on each focus to compute a value respectively
instance Comonad Game where
    extract (Game coords (r, c))= getElem r c coords
    duplicate g@(Game coords focus) = Game games focus where
        games = mapPos makeOne coords
        makeOne p _ = Game coords p

rule :: Game Bool -> Bool
rule g@(Game coords (r, c)) = liveNbrs == 3 || (isAlive && liveNbrs == 2) where
    isAlive = extract g
    liveNbrs = length $ filter id neighbours
    neighbours = isAliveOne <$> neighbourCoords `at` (r, c)
    isAliveOne (r, c) = maybe False id $ safeGet r c coords

next :: Game Bool -> Game Bool
next = extend rule

-- a few helpers 
neighbourCoords = [(row, col) | row <- [-1..1], 
                                col <- [-1..1], 
                                (row, col) /= (0, 0)]

makeGame :: Int -> Int -> [Coord] -> Game Bool
makeGame rows cols whitelist = Game coords (1, 1) where
    coords = matrix rows cols (`elem` whitelist)
    cur = getElem 1 1 coords

fromMatrix :: Matrix Bool -> Game Bool
fromMatrix mtx = Game mtx (1, 1)

-- based on https://github.com/ChrisPenner/conway/blob/master/src/Conway.hs#L61
render :: Game Bool -> String
render (Game coords _) = toBlock $ toLists coords where
    toBlock = foldMap ((++ "\n").toLine)
    toLine = foldMap (bool "." "#")

glider, blinker, beacon :: [Coord]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

at :: [Coord] -> Coord -> [Coord]
at xs (x, y) = bimap (+ x) (+ y) <$> xs

-- helper to convert content of a pattern file (see the .txt files) to a matrix
type Pattern = String
toMatrix :: Pattern -> Matrix Bool
toMatrix str = fromLists lss where
    lss = ((== 'O') <$>) <$> lines str