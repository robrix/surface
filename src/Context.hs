{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}
module Context where

import Expr

data Declaration = Known Type | Unknown
  deriving (Eq, Show)

data Entry
  = Ty TypeEntry
  | Tm TermEntry
  deriving (Eq, Show)

data TypeEntry = Name := Declaration
  deriving (Eq, Show)
data TermEntry = Name `Is` Scheme
  deriving (Eq, Show)

data Backward a = Backward a :< a | Nil
  deriving (Eq, Foldable, Functor, Show)

type Context = Backward Entry
type Suffix = [TypeEntry]

(<><) :: Context -> Suffix -> Context
context <>< [] = context
context <>< (entry : rest) = context :< Ty entry <>< rest

data Extension = Restore | Replace Suffix
  deriving (Eq, Show)


data Index a = Z | S a
  deriving (Eq, Foldable, Functor, Show)

data Schm a
  = Type Type
  | All (Schm (Index a))
  | LetS Type (Schm (Index a))
  deriving (Eq, Foldable, Functor, Show)

type Scheme = Schm Name
