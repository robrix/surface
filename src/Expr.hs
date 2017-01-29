{-# LANGUAGE DeriveFoldable, DeriveFunctor, GADTs #-}
module Expr where

import Data.Functor.Foldable

data ExprF a where
  Product :: a -> a -> ExprF a
  Sum :: a -> a -> ExprF a
  Function :: a -> a -> ExprF a
  UnitT :: ExprF a
  TypeT :: ExprF a

  Abs :: Name -> a -> ExprF a
  Var :: Name -> ExprF a
  App :: a -> a -> ExprF a

  InL :: a -> ExprF a
  InR :: a -> ExprF a
  Case :: a -> a -> a -> ExprF a

  Pair :: a -> a -> ExprF a
  Fst :: a -> ExprF a
  Snd :: a -> ExprF a

  Unit :: ExprF a
  deriving (Eq, Foldable, Functor, Show)

type Expr = Fix ExprF

type TypeF = ExprF
type Type = Fix TypeF

type TermF = ExprF
type Term = Fix TermF


newtype Name = Name Integer
  deriving (Eq, Show)

unName :: Name -> Integer
unName (Name n) = n
