{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}
module Context where

import Data.Foldable (toList)
import Data.Functor.Classes
import Expr
import Text.Pretty

data Entry
  = Ty Binding
  | Tm TermEntry
  | Sep
  deriving (Eq, Show)

data Binding = Name := Maybe Expr
  deriving (Eq, Show)
data TermEntry = Name ::: Scheme
  deriving (Eq, Show)

infixl 8 :<
data Backward a = Backward a :< a | Nil
  deriving (Eq, Foldable, Functor, Show)

type Context = Backward Entry
type Suffix = [Binding]

infixl 8 <><
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


-- Instances

instance Pretty1 Backward where
  liftPrettyPrec pp d = liftPrettyPrec pp d . toList

instance Pretty Entry where
  prettyPrec d (Ty ty) = prettyPrec d ty
  prettyPrec d (Tm term) = prettyPrec d term
  prettyPrec _ Sep = showChar ';'

instance Pretty Binding where
  prettyPrec d (name := declaration) = showParen (d > 9) $ prettyPrec 0 name . showString " := " . maybe (showString "_") (prettyPrec 10) declaration

instance Pretty TermEntry where
  prettyPrec d (name ::: scheme) = showParen (d > 9) $ prettyPrec 10 name . showString " :: " . prettyPrec 10 scheme

instance Pretty1 Schm where
  liftPrettyPrec _ d (Type ty) = prettyPrec d ty
  liftPrettyPrec pp d (All schm) = showsUnaryWith (liftPrettyPrec (liftPrettyPrec pp)) "All" d schm
  liftPrettyPrec pp d (LetS ty schm) = showsBinaryWith prettyPrec (liftPrettyPrec (liftPrettyPrec pp)) "LetS" d ty schm

instance Pretty1 Index where
  liftPrettyPrec _ _ Z = id
  liftPrettyPrec pp d (S a) = pp d a . showChar '\''
