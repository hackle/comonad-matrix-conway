{-# Language DeriveFunctor #-}

module Less2 where

import Data.List

maxOf :: Int -> [Int] -> [Int]
maxOf keep xs = go [] keep xs where
    go xs1 0 xs = xs1
    go xs1 n xs = let (max1, offset) = choose n xs in go (xs1++[max1]) (n - 1) $ drop (offset + 1) xs
    choose n xs = let m = foldl1 max (take (length xs - n + 1) xs);
                        (Just o) = elemIndex m xs 
                    in (m, o)
