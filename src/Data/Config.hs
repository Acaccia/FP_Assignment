module Data.Config ( Config(..)
                   , askConfig
                   ) where

import Control.Monad
import System.Random (StdGen, mkStdGen)

data Config = Config
  {
    sight       :: Int
  , max_water   :: Int
  , seed        :: StdGen
  , treasure_ll :: Double
  , water_ll    :: Double
  , portal_ll   :: Double
  , lava1_ll    :: Double
  , lava2_ll    :: Double
  } deriving (Show)

data ConfigError = OutOfBounds
                 | PercentageOver100
                 | NotANumber

instance Show ConfigError where
  show OutOfBounds       = "A value is out of bounds"
  show PercentageOver100 = "Sum of all percentages is above 100"
  show NotANumber        = "Input is not a number"

type EitherCE = Either ConfigError

boundCheck :: (Ord a, Show a) => a -> a -> a -> EitherCE a
boundCheck low up n = if n > low && n < up
                      then Right n
                      else Left OutOfBounds

eitherReadNum :: (Num a, Read a) => String -> EitherCE a
eitherReadNum s = case reads s of
  [(x, "")] -> Right x
  _         -> Left NotANumber

checkPercentage :: EitherCE Int -> EitherCE Int -> Int -> EitherCE Int
checkPercentage ex ey z = do
  x <- ex
  y <- ey
  if x + y + z <= 100
    then Right z
    else Left PercentageOver100

toPercent :: Int -> Double
toPercent x = fromIntegral x / 100

ask :: (Num a, Read a) => String -> IO (EitherCE a)
ask question = do
  putStr (question ++ ": ")
  eitherReadNum <$> getLine

askConfig :: IO (Either ConfigError Config)
askConfig = do
  sight <- ask "sight"
  max_water <- ask "max water"
  seed <- fmap mkStdGen <$> (ask "seed")
  treasure_ll <- (>>= boundCheck 0 100) <$> ask "treasure likelihood"
  water_ll <- (>>= boundCheck 0 100) <$> ask "water likelihood"
  portal_ll <- (>>= boundCheck 0 100) <$> ask "portal likelihood"
  lava1_ll <- (>>= boundCheck 0 100 >=> checkPercentage water_ll portal_ll) <$> ask "lava likelihood"
  lava2_ll <- (>>= boundCheck 0 100 >=> checkPercentage water_ll portal_ll) <$> ask "lava (adjacent) likelihood"
  pure $ Config <$> sight <*> max_water <*> seed <*> treasure_ll
    <*> fmap toPercent water_ll <*> fmap toPercent portal_ll
    <*> fmap toPercent lava1_ll <*> fmap toPercent lava2_ll
