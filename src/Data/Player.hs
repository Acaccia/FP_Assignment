module Data.Player where

import Data.Internal.Nat

data Player = Player {
    xPos  :: Nat
  , yPos  :: Nat
  , chest :: Nat
  , water :: Nat
  } deriving Show

data Direction = U | D | L | R deriving Show

move :: Direction -> Player -> Player
move U p = p {xPos = xPos p - 1, water = water p - 1}
move D p = p {xPos = xPos p + 1, water = water p - 1}
move L p = p {yPos = yPos p - 1, water = water p - 1}
move R p = p {yPos = yPos p + 1, water = water p - 1}

addChest :: Player -> Player
addChest p = p {chest = chest p + 1}

refillWater :: Nat -> Player -> Player
refillWater n p = p {water = n}
