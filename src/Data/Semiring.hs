module Data.Semiring where

class Monoid m => Semiring m where
  one :: m
  (><) :: m -> m -> m
