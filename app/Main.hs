module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Lib

tickTime :: Int
tickTime = 200000

game = makeGame 20 20 $ glider `at` (0, 0)

main :: IO ()
main = forM_ (iterate (next rule) game) $ \g -> do
  putStr "\ESC[2J" -- Clear terminal screen
  putStrLn (render 10 g)
  threadDelay tickTime