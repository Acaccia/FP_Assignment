module Data.Config ( Config(..)
                   , askConfig
                   ) where

import Control.Monad
import System.Random (StdGen, mkStdGen)

data Config = Config {
    sight      :: Int
  , maxWater   :: Int
  , seed       :: StdGen
  , treasureLL :: Double
  , waterLL    :: Double
  , portalLL   :: Double
  , lava1LL    :: Double
  , lava2LL    :: Double
  } deriving (Show)

data ConfigError = OutOfBounds String String String
                 | PercentageOver100
                 | NotANumber String

instance Show ConfigError where
  show (OutOfBounds q l u) = q ++ ": value is out of bounds [" ++ l ++ " - " ++ u ++ "]"
  show PercentageOver100   = "Sum of all percentages is above 100"
  show (NotANumber q)      = q ++ ": input is not a number"

type EitherCE = Either ConfigError

boundCheck :: (Ord a, Show a) => String -> a -> a -> a -> EitherCE a
boundCheck q low up n = if n > low && n < up
                        then Right n
                        else Left $ OutOfBounds q (show low) (show up)

eitherReadNum :: (Num a, Read a) => String -> String -> EitherCE a
eitherReadNum s q = case reads s of
  [(x, "")] -> Right x
  _         -> Left $ NotANumber q

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
  eitherReadNum question <$> getLine

askConfig :: IO (Either ConfigError Config)
askConfig = do
  sight <- ask "sight"
  max_water <- ask "max water"
  seed <- fmap mkStdGen <$> ask "seed"
  treasure_ll <- (>>= boundCheck "treasure likelihood" 0 100) <$> ask "treasure likelihood"
  water_ll <- (>>= boundCheck "water likelihood" 0 100) <$> ask "water likelihood"
  portal_ll <- (>>= boundCheck "portal likelihood" 0 100) <$> ask "portal likelihood"
  lava1_ll <- (>>= boundCheck "lava likelihood" 0 100 >=> checkPercentage water_ll portal_ll)
              <$> ask "lava likelihood"
  lava2_ll <- (>>= boundCheck "lava (adjacent) likelihood" 0 100 >=> checkPercentage water_ll portal_ll)
              <$> ask "lava (adjacent) likelihood"
  pure $ Config <$> sight <*> max_water <*> seed <*> treasure_ll
    <*> fmap toPercent water_ll <*> fmap toPercent portal_ll
    <*> fmap toPercent lava1_ll <*> fmap toPercent lava2_ll
