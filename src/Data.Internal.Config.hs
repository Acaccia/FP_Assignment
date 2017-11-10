module Data.Internal.Config ( Config(..)
                            , askConfig
                            ) where

import Control.Monad
import System.Random (StdGen, mkStdGen)

data Config = Config
  {
    sight       :: Int
  , max_water   :: Int
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

ask :: (Num a, Read a) => IO (EitherCE a)
ask = eitherReadNum <$> getLine

askConfig :: IO (Either ConfigError Config)
askConfig = do
  putStr "sight: "
  sight <- ask
  putStr "max water: "
  max_water <- ask
  putStr "treasure likelihood: "
  treasure_ll <- (>>= boundCheck 0 100) <$> ask
  putStr "water likelihood: "
  water_ll <- (>>= boundCheck 0 100) <$> ask
  putStr "portal likelihood: "
  portal_ll <- (>>= boundCheck 0 100) <$> ask
  putStr "lava likelihood: "
  lava1_ll <- (>>= boundCheck 0 100 >=> checkPercentage water_ll portal_ll) <$> ask
  putStr "lava (adjacent) likelihood: "
  lava2_ll <- (>>= boundCheck 0 100 >=> checkPercentage water_ll portal_ll) <$> ask
  pure $ Config <$> sight <*> max_water <*> treasure_ll <*> fmap toPercent water_ll <*> fmap toPercent portal_ll <*> fmap toPercent lava1_ll <*> fmap toPercent lava2_ll
