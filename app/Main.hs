-- this is mostly copied from https://github.com/ChrisPenner/conway/blob/master/app/Main.hs
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Game

tickTime :: Int
tickTime = 200000

game = makeGame 50 150 $ 
    glider `at` (15, 1) 
    ++ blinker `at` (25, 25)
    ++ beacon `at` (35, 75)

main :: IO ()
main = forM_ (iterate next game) $ \g -> do
  putStr "\ESC[2J" -- Clear terminal screen
  putStrLn (render g)
  threadDelay tickTime