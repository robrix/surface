module Data.Semiring
( zero
, Semiring(..)
, Semigroup(..)
) where

import Data.Semigroup

zero :: Monoid m => m
zero = mempty

class (Semigroup m, Monoid m) => Semiring m where
  one :: m

  infixr 7 ><
  (><) :: m -> m -> m
