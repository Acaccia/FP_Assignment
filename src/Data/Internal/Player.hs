module Data.Internal.Player where

import Data.Internal.Nat

data Player = Player {
    x_pos :: Nat
  , y_pos :: Nat
  , treasures :: Nat
  , water :: Nat
  } deriving Show

data Direction = U | D | L | R deriving Show

move :: Direction -> Player -> Player
move U p = p {x_pos = x_pos p - 1, water = water p - 1}
move D p = p {x_pos = x_pos p + 1, water = water p - 1}
move L p = p {y_pos = y_pos p - 1, water = water p - 1}
move R p = p {y_pos = y_pos p + 1, water = water p - 1}
