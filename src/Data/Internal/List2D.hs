module Data.Internal.List2D where

import Data.Internal.Nat

newtype List2D a = List2D [[a]]

type Index = (Nat, Nat)

-- List2D

instance Functor List2D where
  fmap f (List2D l) = List2D $ (fmap f) <$> l

instance Show a => Show (List2D a) where
  show (List2D a) = show a

(!) :: List2D a -> Index -> a
List2D xs ! (Nat i, Nat j) = xs !! i !! j

set :: a -> Index -> List2D a -> List2D a
set a (Nat i, Nat j) (List2D l2d) =
  let (xs, y:ys) = splitAt i l2d
      (xxs, _:yys) = splitAt j y
  in List2D $ xs ++ [xxs ++ [a] ++ yys] ++ ys
