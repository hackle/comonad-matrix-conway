-- this is mostly copied from https://github.com/ChrisPenner/conway/blob/master/app/Main.hs
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Game
import System.Directory
import Footy

tickTime :: Int
tickTime = 50000

-- game = makeGame 50 150 $ 
--     glider `at` (15, 1) 
--     ++ blinker `at` (25, 25)
--     ++ beacon `at` (35, 75)

fromFile :: IO (Game Bool)
fromFile = do
    d <- getCurrentDirectory
    text <- readFile $ d ++ "/src/trueperiod22gun.txt"
    pure 
        $ fromMatrix 
        $ toMatrix text 

main :: IO ()
main = do
    game <- fromFile
    forM_ (iterate next game) $ \g -> do
        putStr "\ESC[2J" -- Clear terminal screen
        putStrLn (render g)
        threadDelay tickTime

