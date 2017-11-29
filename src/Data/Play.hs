{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Data.Play where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Control.Parallel.Strategies
import qualified Data.Config                     as Conf
import           Data.Desert
import           Data.Functor
import           Data.Internal.List2D.BFS.Strict
import           Data.Internal.Nat
import           Data.Player

data Play = Play { _player :: Player
                 , _desert :: Desert
                 }
makeLenses ''Play

data InGameConfig = IGConfig { _maxWater :: Int
                             , _sight    :: Int
                             }

makeLenses ''InGameConfig

data Situation = Win Int | Lost | Ongoing

initPlay :: Conf.Config -> Play
initPlay (Conf.Config _ mw seed t w p l ll) =
   Play (Player (0, 0) 0 mw) (makeDesert t w p l ll seed)

inGameConfig :: Conf.Config -> InGameConfig
inGameConfig = IGConfig <$> Conf.maxWater <*> Conf.sight

movePlayer :: MonadState Play m => Char -> m ()
movePlayer 'w' = player %= move U
movePlayer 'a' = player %= move L
movePlayer 's' = player %= move D
movePlayer 'd' = player %= move R
movePlayer _   = pure ()

reactToTile :: (MonadState Play m, MonadReader InGameConfig m) => m Situation
reactToTile = do
  pos <- uses player pos
  (! pos) <$> use desert >>= \case
    Sand True -> do desert %= openChest pos
                    player %= addChest
                    pure Ongoing
    Water     -> do w <- view maxWater
                    player %= refillWater w
                    pure Ongoing
    Lava      -> pure Lost
    Portal    -> Win <$> uses player chest
    _         -> pure Ongoing

checkWater :: MonadState Play m => m Situation
checkWater = chk <$> uses player water
  where chk 0 = Lost
        chk _ = Ongoing

getClosest :: (Nat, Nat) -> Desert -> (Maybe Nat, Maybe Nat, Maybe Nat)
getClosest pos d = runEval $ do
  closestW <- rpar $ bfsDistance Water [Lava] pos d
  closestT <- rpar $ bfsDistance (Sand True) [Lava] pos d
  closestP <- rpar $ bfsDistance Portal [Lava] pos d
  pure (closestW, closestT, closestP)

printGame :: (MonadIO m, MonadReader InGameConfig m, MonadState Play m) => m ()
printGame = do
    s <- view sight
    Player pos c w <- use player
    d <- use desert
    let (closestW, closestT, closestP) = getClosest pos d
    liftIO (putStr $ observe pos s d)
    liftIO (putStrLn $ "Collected treasures: " ++ show c)
    liftIO (putStrLn $ "Remaining water: " ++ show w)
    liftIO (putStrLn $ "Closest water: " ++ show closestW)
    liftIO (putStrLn $ "Closest treasure: " ++ show closestT)
    liftIO (putStrLn $ "Closest portal: " ++ show closestP)