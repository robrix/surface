module Data.Semiring where

import Data.Function
import Data.Semigroup

zero :: Monoid m => m
zero = mempty

class (Semigroup m, Monoid m) => Semiring m where
  one :: m
  (><) :: m -> m -> m


newtype NumRing n = NumRing { unNumRing :: n }

instance Num n => Semigroup (NumRing n) where
  (<>) = (NumRing .) . ((+) `on` unNumRing)

instance Num n => Monoid (NumRing n) where
  mempty = NumRing 0
  mappend = (<>)

instance Num n => Semiring (NumRing n) where
  one = NumRing 1
  (><) = (NumRing .) . ((*) `on` unNumRing)
