module Data.Internal.List2D.BFS.Lazy (bfsDistance) where

import           Control.Monad.State
import           Data.Internal.Direction
import           Data.Internal.List2D
import           Data.Sequence           (Seq, ViewL (..), fromList, viewl,
                                          (><))
import qualified Data.Set                as S
import           Debug.Trace

type Position = (Nat, Nat)
type BFSState = (Seq (Position, Nat), S.Set Position)

bfsDistance :: (Show a, Eq a) => a -> [a] -> Position -> List2D a -> Maybe Nat
bfsDistance target avoid pos ls = if ls ! pos == target then Just 0
        else evalState go (fromList [(p, 1) | d <- [minBound..maxBound], let p = move d pos, p /= pos], S.empty)
  where go :: State BFSState (Maybe Nat)
        go = do (s, visited) <- get
                case viewl s of
                  EmptyL        -> pure Nothing
                  ((p, n) :< t) -> let tile = ls ! p
                                       visited' = S.insert p visited
                       in if tile == target then pure (Just n)
                          else if tile `elem` avoid then put (t, visited') >> go
                          else put (t >< fromList [(p', n+1) | d <- [minBound..maxBound], let p' = move d p, p' /= p && p' `S.notMember` visited], visited') >> go
