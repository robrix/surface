module Data.Semiring where

zero :: Monoid m => m
zero = mempty

class Monoid m => Semiring m where
  one :: m
  (><) :: m -> m -> m
