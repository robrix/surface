{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}
module Context where

import Data.Foldable (toList)
import Expr
import Text.Pretty

data Constraint
  = Ty DefinitionConstraint
  | Tm TypeConstraint
  | Sep
  deriving (Eq, Show)

data DefinitionConstraint = Name := Maybe Expr
  deriving (Eq, Show)
data TypeConstraint = Name ::: Expr
  deriving (Eq, Show)

infixl 8 :<
data Backward a = Backward a :< a | Nil
  deriving (Eq, Foldable, Functor, Show)

type Context = Backward Constraint
type Suffix = [DefinitionConstraint]

infixl 8 <><
(<><) :: Context -> Suffix -> Context
context <>< [] = context
context <>< (entry : rest) = context :< Ty entry <>< rest

applyContext :: Expr -> Context -> Expr
applyContext expr context = case context of
  Nil -> expr
  (rest :< Ty (name := d)) | Just t <- d -> applyContext (substitute t name expr) rest
  (rest :< _) -> applyContext expr rest


data Extension = Restore | Replace Suffix
  deriving (Eq, Show)


-- Instances

instance Pretty1 Backward where
  liftPrettyPrec pp pl d = liftPrettyPrec pp pl d . toList

instance Pretty Constraint where
  prettyPrec d (Ty ty) = prettyPrec d ty
  prettyPrec d (Tm term) = prettyPrec d term
  prettyPrec _ Sep = showChar ';'

instance Pretty DefinitionConstraint where
  prettyPrec d (name := declaration) = showParen (d > 9) $ prettyPrec 0 name . showString " := " . maybe (showString "_") (prettyPrec 10) declaration

instance Pretty TypeConstraint where
  prettyPrec d (name ::: scheme) = showParen (d > 9) $ prettyPrec 10 name . showString " :: " . prettyPrec 10 scheme
