{-# LANGUAGE MultiWayIf #-}
module Desert where

import Control.Monad.State
import Data.Bifunctor
import Data.Internal.List2D
import System.Random

data Tile = Sand Bool | Water | Lava | Portal deriving (Eq, Show)

type Desert = List2D Tile

makeDesert :: Double -> Double -> Double -> Double -> Double -> StdGen -> Desert
makeDesert t w p l ll g = List2D (headLine : tailLines)
  where

    (s:seeds) = mkStdGen <$> randoms g

    randomTile :: State (StdGen, Tile, [Tile]) Tile
    randomTile = do
      (g, tileLeft, (tileUp : ts)) <- get
      let (r, g') = random g
      let l' = if Lava `elem` [tileLeft, tileUp] then ll else l
      let p' = p + w
      let (tile, gg) = if | r < w      -> (Water, g')
                          | r < p'     -> (Portal, g')
                          | r < p' + l -> (Lava, g')
                          | otherwise  -> first (Sand . (< t)) $ random g'
      put (gg, tile, ts)
      pure tile

    lineOfTiles :: State (StdGen, Tile, [Tile]) [Tile]
    lineOfTiles = (:) <$> randomTile <*> lineOfTiles

    headLine :: [Tile]
    headLine = Sand False : evalState lineOfTiles (s, Sand False, repeat (Sand False))

    tailLines :: [[Tile]]
    tailLines = evalState lineOfTiles <$> (zipWith3 (,,) seeds (repeat $ Sand False) (headLine : tailLines))
