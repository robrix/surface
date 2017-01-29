{-# LANGUAGE DeriveFunctor #-}
module Data.Result where

import Control.Applicative

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

instance Alternative (Result e) where
  empty = Error []
  Error s1 <|> Error s2 = Error (s1 ++ s2)
  Result a <|> _ = Result a
  _ <|> Result b = Result b
