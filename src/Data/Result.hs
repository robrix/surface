{-# LANGUAGE DeriveFunctor #-}
module Data.Result where

import Control.Applicative
import Text.Pretty

data Result a = Result a | Error [String]
  deriving (Eq, Functor, Show)


-- Instances

instance Applicative Result where
  pure = Result
  Error s <*> _ = Error s
  Result f <*> a = fmap f a

instance Monad Result where
  return = pure
  Error s >>= _ = Error s
  Result a >>= f = f a

instance Alternative Result where
  empty = Error []
  Error s1 <|> Error s2 = Error (s1 ++ s2)
  Result a <|> _ = Result a
  _ <|> Result b = Result b

instance Pretty a => Pretty (Result a) where
  prettyPrec (Result a) = prettyPrec a
  prettyPrec (Error errors) = (0, foldr (.) id (fmap (\ e -> showString e . showChar '\n') errors))
