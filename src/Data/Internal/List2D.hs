module Data.Internal.List2D (List2D(..), (!), set, Nat) where

import Data.Internal.Nat
import Text.Printf

newtype List2D a = List2D [[a]]

type Index = (Nat, Nat)

instance Functor List2D where
  fmap f (List2D l) = List2D $ fmap f <$> l

instance Show a => Show (List2D a) where
  show (List2D a) = show a

(!) :: List2D a -> Index -> a
List2D xs ! (i, j) = xs !! fromEnum i !! fromEnum j

set :: a -> Index -> List2D a -> List2D a
set a (i, j) (List2D l2d) =
  let (xs, y:ys) = splitAt (fromEnum i) l2d
      (xxs, _:yys) = splitAt (fromEnum j) y
  in List2D $ xs ++ [xxs ++ [a] ++ yys] ++ ys

--observe :: Show a => Index -> Nat -> List2D a -> String
observe (x, y) sight (List2D grid) = xs
  where x' = fromEnum x
        y' = fromEnum y
        sight' = fromEnum sight
        xxs = dropAndTake (x'-sight') (2*sight + 1) grid
        xs = zipWith takeAround ([0..sight'] ++ [sight'-1, sight'-2..]) xxs

        dropAndTake :: Int -> Int -> [[a]] -> [[a]]
        dropAndTake d t xs = if d < 0
          then replicate (abs d) [] ++ take (t+d) xs
          else take t (drop d xs)

        takeAround :: Int -> [a] -> [a]
        takeAround n = drop (y'-n) . take (y'+n+1)

test :: List2D (Nat, Nat)
test = List2D [[(x, y) | y <- [0..]] | x <- [0..]]
