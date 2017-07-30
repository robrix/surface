{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances #-}
module Context where

import Data.Foldable (toList)
import Expr
import Text.Pretty

data Constraint s a
  = D (DefinitionConstraint s a)
  | T (TypeConstraint s a)
  | Sep
  deriving (Eq, Foldable, Functor, Show, Traversable)

data DefinitionConstraint s a = Name := Maybe a
  deriving (Eq, Foldable, Functor, Show, Traversable)
data TypeConstraint s a = Name ::: a
  deriving (Eq, Foldable, Functor, Show, Traversable)

infixl 8 :<
data Backward a = Backward a :< a | Nil
  deriving (Eq, Foldable, Functor, Show, Traversable)

type Context s a = Backward (Constraint s a)
type Suffix a = [DefinitionConstraint () a]

infixl 8 <><
(<><) :: Context () a -> Suffix a -> Context () a
context <>< [] = context
context <>< (entry : rest) = context :< D entry <>< rest

applyContext :: Expr -> Context s Expr -> Expr
applyContext expr context = case context of
  Nil -> expr
  (rest :< D (name := d)) | Just t <- d -> applyContext (substitute t name expr) rest
  (rest :< _) -> applyContext expr rest


data Extension a = Restore | Replace (Suffix a)
  deriving (Eq, Foldable, Functor, Show, Traversable)


-- Instances

instance Pretty1 Backward where
  liftPrettyPrec pp pl d = liftPrettyPrec pp pl d . toList

instance Pretty a => Pretty (Backward a) where
  prettyPrec = prettyPrec1

instance Pretty s => Pretty (Constraint s Expr) where
  prettyPrec d (D ty) = prettyPrec d ty
  prettyPrec d (T term) = prettyPrec d term
  prettyPrec _ Sep = showChar ';'

instance Pretty s => Pretty (DefinitionConstraint s Expr) where
  prettyPrec d (name := declaration) = showParen (d > 9) $ prettyPrec 0 name . showString " := " . maybe (showString "_") (prettyExpr 10) declaration

instance Pretty s => Pretty (TypeConstraint s Type) where
  prettyPrec d (name ::: scheme) = showParen (d > 9) $ prettyPrec 10 name . showString " :: " . prettyExpr 10 scheme
