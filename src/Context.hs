{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances #-}
module Context where

import Data.Foldable (toList)
import Expr
import Text.Pretty

data Constraint a
  = D (DefinitionConstraint a)
  | T (TypeConstraint a)
  | Sep
  deriving (Eq, Foldable, Functor, Show, Traversable)

data DefinitionConstraint a = Name := Maybe a
  deriving (Eq, Foldable, Functor, Show, Traversable)
data TypeConstraint a = Name ::: a
  deriving (Eq, Foldable, Functor, Show, Traversable)

infixl 8 :<
data Backward a = Backward a :< a | Nil
  deriving (Eq, Foldable, Functor, Show, Traversable)

type Context a = Backward (Constraint a)
type Suffix a = [DefinitionConstraint a]

infixl 8 <><
(<><) :: Context a -> Suffix a -> Context a
context <>< [] = context
context <>< (entry : rest) = context :< D entry <>< rest

applyContext :: Expr -> Context Expr -> Expr
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

instance Pretty1 Constraint where
  liftPrettyPrec ppr plr d c = case c of
    D c -> liftPrettyPrec ppr plr d c
    T c -> liftPrettyPrec ppr plr d c
    Sep -> showChar ';'

instance Pretty1 DefinitionConstraint where
  liftPrettyPrec ppr _ d (name := def) = showParen (d > 9) $ prettyPrec 0 name . showString " := " . maybe (showChar '_') (ppr 10) def

instance Pretty1 TypeConstraint where
  liftPrettyPrec ppr _ d (name ::: ty) = showParen (d > 9) $ prettyPrec 10 name . showString " :: " . ppr 10 ty
