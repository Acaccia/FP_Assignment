module Data.Internal.List2D.BFS.Lazy (bfsDistance) where

import Control.Monad.State
import Data.Internal.Direction
import Data.Internal.List2D
import Data.Sequence           (Seq, ViewL (..), fromList, viewl, (><))

type Position = (Nat, Nat)
type BFSState = Seq (Position, Direction, Nat)

bfsDistance :: Eq a => a -> [a] -> Position -> List2D a -> Maybe Nat
bfsDistance target avoid pos ls = if ls ! pos == target then Just 0
        else evalState go (fromList [(p, d, 1) | d <- [minBound..maxBound], let p = move d pos, p /= pos])
  where go :: State BFSState (Maybe Nat)
        go = get >>= \s -> case viewl s of
                EmptyL               -> pure Nothing
                ((p, prevD, n) :< t) -> let tile = ls ! p in
                  if tile == target then pure (Just n)
                    else if tile `elem` avoid then go
                    else put (t >< fromList [(p', opposed d, n+1) | d <- [minBound..maxBound], let p' = move d p, p' /= p && d /= prevD]) >> go
