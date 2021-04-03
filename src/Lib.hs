{-# Language GADTs #-}
{-# Language StandaloneDeriving #-}
{-# Language KindSignatures #-}
{-# Language DeriveFunctor #-}

module Lib where

import Data.Bifunctor
import Data.List.NonEmpty hiding (filter, length)
import Control.Comonad
import Prelude hiding ((!!), head, take)
import Data.Bool
import Data.List.Split

type Coord = (Int, Int, Bool)
data Game a = Game { cells :: NonEmpty a, cur :: a } deriving (Functor, Show)

instance Comonad Game where
    extract = cur
    duplicate g@(Game xs x) = Game gs g where
        gs = mkGame <$> xs
        mkGame x1 = Game xs x1 

rule :: Game Coord -> Coord
rule g@(Game xs (x, y, isAlive)) = (x, y, toLive) where
    toLive = liveNeighbours == 3 || (isAlive && liveNeighbours == 2)
    liveNeighbours = length $ filter (\(_, _, b) -> b) neighbours
    neighbours = filter (\(x1, y1, _) -> (x1, y1) `elem` xys) $ toList xs
    xys = [ bimap (+ x) (+ y) (ox, oy) | ox <- [-1..1], oy <- [-1..1], (ox, oy) /= (0, 0) ] 

withCoords :: Int -> Int -> [(Int, Int)] -> [Coord]
withCoords cols rows trues = [(c, r, isTrue c r) | c <- [0..cols], r <- [0..rows] ] where
    isTrue c r = (c, r) `elem` trues

makeGame :: Int -> Int -> [(Int, Int)] -> Game Coord
makeGame c r t = Game xs x where
    xs = fromList $ withCoords c r t
    x = head xs

at :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
at xs (x, y) = bimap (+ x) (+ y) <$> xs

glider, blinker, beacon :: [(Int, Int)]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

render :: Int -> Game Coord -> String
render cols (Game xs x) = 
    foldMap (++ "\n")
    $ chunksOf cols 
    $ ((bool '.' '#') . third) <$> toList xs where
        third (_, _, v) = v

next :: (Game Coord -> Coord) -> Game Coord -> Game Coord
next = extend