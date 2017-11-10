{-# LANGUAGE MultiWayIf #-}
module Desert where

import System.Random
import Control.Monad.State
import Data.Internal.List2D

data Tile = Sand Bool | Water | Lava | Portal deriving Show

type Desert = List2D Tile

makeDesert :: StdGen -> Desert
makeDesert g = undefined

randomTile :: Double -> Double -> Double -> Double -> State StdGen Tile
randomTile tll wll pll lll = do
  (r, g) <- random <$> get
  put g
  if | r < wll -> pure Water
     | r < pll -> pure Portal
     | r < lll -> pure Lava
     | otherwise -> do
        (r, g') <- random <$> get
        put g'
        pure $ Sand (r < tll)
