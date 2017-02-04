{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}
module Context where

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
  liftPrettyPrec pp d (Nil :< h) = pp d h
  liftPrettyPrec pp d (t :< h) = showParen (d > 8) (liftPrettyPrec pp 8 t . showString " :< " . pp 9 h)
  liftPrettyPrec _ _ Nil = showString "Nil"

instance Pretty TypeEntry where
  prettyPrec d (name := declaration) = showParen (d > 9) $ prettyPrec 9 name . showString " := " . prettyPrec 10 declaration

instance Pretty Declaration where
  prettyPrec d (Known ty) = showChar '!' . prettyPrec d ty
  prettyPrec _ _ = showChar '?'
