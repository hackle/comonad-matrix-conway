{-# Language DeriveFunctor #-}

module Game where

import Data.Bifunctor
import Control.Comonad
import Data.Bool
import Data.Matrix

type Coord = (Int, Int)
data Game a = Game { 
    coords :: Matrix a
    , focus :: (Coord, a) 
    } deriving (Functor, Show)

-- the trick of contextual computation with Comonads is all in 
-- duplicate + extend
-- duplicate distributes the focus 
-- and extend acts on each focus to compute a value respectively

instance Comonad Game where
    extract = snd . focus
    -- duplicate distributes the focus onto every coordinate
    duplicate g@(Game coords (pos, _)) = Game games (pos, g) where
        games = mapPos makeOne coords
        makeOne p v = Game coords (p, v) 

rule :: Game Bool -> Bool
rule (Game coords ((r, c), isAlive)) = liveNbrs == 3 || (isAlive && liveNbrs == 2) where
    liveNbrs = length $ filter id neighbours
    neighbours = isAliveOne <$> neighbourCoords `at` (r, c)
    isAliveOne (r, c) = maybe False id $ safeGet r c coords

next :: Game Bool -> Game Bool
next = extend rule

toMatrix :: String -> Matrix Bool
toMatrix str = fromLists lss where
    lss = ((== 'O') <$>) <$> lines str

-- a few helpers copied from https://github.com/ChrisPenner/conway/blob/master/src/Conway.hs#L61

neighbourCoords = [(row, col) | row <- [-1..1], 
                                col <- [-1..1], 
                                (row, col) /= (0, 0)]

makeGame :: Int -> Int -> [Coord] -> Game Bool
makeGame rows cols whitelist = Game coords ((1, 1), cur) where
    coords = matrix rows cols (`elem` whitelist)
    cur = getElem 1 1 coords

fromMatrix :: Matrix Bool -> Game Bool
fromMatrix mtx = Game mtx ((1, 1), getElem 1 1 mtx)

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
