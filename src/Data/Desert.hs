module Desert where

import System.Random
import Control.Monad.State

data Tile = Sand Bool | Water | Lava | Portal deriving Show

