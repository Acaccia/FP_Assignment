module Data.Player (Player(..), D.Direction(..), move, addChest, refillWater) where

import qualified Data.Internal.Direction as D
import           Data.Internal.Nat

data Player = Player {
    pos   :: (Nat, Nat)
  , chest :: Int
  , water :: Int
  } deriving Show

move :: D.Direction -> Player -> Player
move d p = p {pos = D.move d (pos p), water = water p - 1}

addChest :: Player -> Player
addChest p = p {chest = chest p + 1}

refillWater :: Int -> Player -> Player
refillWater n p = p {water = n}
