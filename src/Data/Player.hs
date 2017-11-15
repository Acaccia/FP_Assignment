module Data.Player where

import qualified Data.Internal.Direction as D
import           Data.Internal.Nat

data Player = Player {
    pos   :: (Nat, Nat)
  , chest :: Nat
  , water :: Nat
  } deriving Show

move :: D.Direction -> Player -> Player
move d p = p {pos = D.move d (pos p), water = water p - 1}

addChest :: Player -> Player
addChest p = p {chest = chest p + 1}

refillWater :: Nat -> Player -> Player
refillWater n p = p {water = n}
