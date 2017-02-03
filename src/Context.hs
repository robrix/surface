{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}
module Context where

import Expr

data Declaration = Known Type | Unknown
  deriving (Eq, Show)

data Entry = Name := Declaration
  deriving (Eq, Show)

data Backward a = Backward a :< a | Nil
  deriving (Eq, Foldable, Functor, Show)

type Context = Backward Entry
type Suffix = [Entry]
