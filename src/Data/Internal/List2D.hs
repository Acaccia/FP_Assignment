module List2D where

import Data.Internal.Nat

newtype List2D a = List2D [[a]]

instance Functor List2D where
  fmap f (List2D l) = List2D $ (fmap f) <$> l

