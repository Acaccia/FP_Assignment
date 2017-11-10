module Data.Internal.Nat where

newtype Nat = Nat Int

instance Num Nat where
  fromInteger = Nat . fromInteger
  Nat i + Nat j = Nat (i + j)
  (-) = error "not handled"
  Nat i * Nat j = Nat (i * j)
  abs n = n
  signum (Nat n) = Nat (signum n)

instance Show Nat where show (Nat n) = show n
instance Read Nat where
  readsPrec n = fmap (first fromInteger) . readsPrec n
    where first f (a, b) = (f a, b)

instance Enum Nat where
  toEnum = Nat
  fromEnum (Nat n) = n

instance Bounded Nat where
  minBound = 0
  maxBound = Nat maxBound
