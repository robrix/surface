{-# LANGUAGE DeriveFunctor #-}
module Data.Result where

data Result e a = Result a | Error [e]
  deriving (Eq, Functor, Show)


-- Instances

instance Applicative (Result e) where
  pure = Result
  Error s <*> _ = Error s
  Result f <*> a = fmap f a

instance Monad (Result e) where
  return = pure
  Error s >>= _ = Error s
  Result a >>= f = f a
