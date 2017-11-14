{-# LANGUAGE MultiWayIf #-}
module Desert where

import Control.Monad.State
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
        let lavaNear = tileLeft == Lava || tileUp == Lava
        (tile, gg) <- if | r < w     -> pure (Water, g')
                         | r < p'    -> pure (Portal, g')
                         | r < p' + (if lavaNear then ll else l)
                                     -> pure (Lava, g')
                         | otherwise -> do
                             let (r', g'') = random g'
                             pure (Sand (r' < t), g'')
        put (gg, tile, ts)
        pure tile
      where p' = w + p

    lineOfTiles :: State (StdGen, Tile, [Tile]) [Tile]
    lineOfTiles = (:) <$> randomTile <*> lineOfTiles

    headLine :: [Tile]
    headLine = Sand False : evalState lineOfTiles (s, Sand False, repeat (Sand False))

    tailLines :: [[Tile]]
    tailLines = evalState lineOfTiles <$> (zipWith3 (,,) seeds (repeat $ Sand False) (headLine : tailLines))
