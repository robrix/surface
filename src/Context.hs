{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}
module Context where

import Data.Foldable (toList)
import Expr
import Text.Pretty

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

infixl 8 :<
data Backward a = Backward a :< a | Nil
  deriving (Eq, Foldable, Functor, Show)

type Context = Backward Entry
type Suffix = [TypeEntry]

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

instance Pretty TypeEntry where
  prettyPrec d (name := declaration) = showParen (d > 9) $ prettyTypeName name . showString " := " . prettyPrec 10 declaration

instance Pretty Declaration where
  prettyPrec d (Known ty) = showChar '!' . prettyPrec d ty
  prettyPrec _ _ = showChar '?'

instance Pretty TermEntry where
  prettyPrec d (name `Is` scheme) = showParen (d > 9) $ prettyTermName name . showString " :: " . prettyPrec 10 scheme

instance Pretty1 Schm where
  liftPrettyPrec _ d (Type ty) = prettyType d ty
  liftPrettyPrec pp d (All schm) = showParen (d > 10) $ showString "All " .  liftPrettyPrec (liftPrettyPrec pp) 10 schm
  liftPrettyPrec pp d (LetS ty schm) = showParen (d > 10) $ showString "LetS " . prettyPrec 10 ty . showChar ' ' . liftPrettyPrec (liftPrettyPrec pp) 10 schm

instance Pretty1 Index where
  liftPrettyPrec _ _ Z = id
  liftPrettyPrec pp d (S a) = pp d a . showChar '\''
