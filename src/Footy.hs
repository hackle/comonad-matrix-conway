module Footy where

import Prelude hiding (take)
import Data.List.NonEmpty hiding (cycle)
import Control.Comonad

players = fromList $ cycle [ 'A' .. 'E' ]

-- It's cheeky but we know for sure 
-- there are infinite number of elements with "cycle"
makePass :: NonEmpty Char -> String
makePass (a :| b : _) = a : " passes the ball to " ++ [ b ]

-- NonEmpty is an instance of Comonad
-- extend/duplicate will build a NonEmpty of (inner) NonEmpty, with the "head" moving forward
-- https://hackage.haskell.org/package/comonad-5.0.8/docs/src/Control.Comonad.html#line-206
passes = toList $ extend makePass players

-- try take 7 passes