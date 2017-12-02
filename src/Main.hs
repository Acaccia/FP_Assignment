module Main where

import Control.Monad.Reader
import Control.Monad.State
import Data.Config
import Data.Play
import System.IO

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  config <- askConfigUntilUserBecomesClever
  let play = initPlay config
  let igConf = inGameConfig config
  runReaderT (evalStateT gameLoop play) igConf
