{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances #-}
module Context where

import Data.Bifunctor
import Data.Foldable (toList)
import Data.Semiring
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
type Suffix s a = [DefinitionConstraint s a]

infixl 8 <><
(<><) :: Context s a -> Suffix s a -> Context s a
context <>< [] = context
context <>< (entry : rest) = context :< D entry <>< rest

applyContext :: Expr -> Context s Expr -> Expr
applyContext expr context = case context of
  Nil -> expr
  (rest :< D (name := d)) | Just t <- d -> applyContext (substitute t name expr) rest
  (rest :< _) -> applyContext expr rest

scaleContext :: Semiring s => s -> Context s a -> Context s a
scaleContext s = fmap (first (s ><))


data Extension s a = Restore | Replace (Suffix s a)
  deriving (Eq, Foldable, Functor, Show, Traversable)


-- Instances

instance Bifunctor DefinitionConstraint where
  bimap _ g (name := a) = name := fmap g a

instance Bifunctor TypeConstraint where
  bimap _ g (name ::: ty) = name ::: g ty

instance Bifunctor Constraint where
  bimap f g (D c) = D (bimap f g c)
  bimap f g (T c) = T (bimap f g c)
  bimap _ _ Sep = Sep


instance Pretty1 Backward where
  liftPrettyPrec pp pl d = liftPrettyPrec pp pl d . toList

instance Pretty a => Pretty (Backward a) where
  prettyPrec = prettyPrec1

instance Pretty2 Constraint where
  liftPrettyPrec2 ppa pla ppr plr d c = case c of
    D c -> liftPrettyPrec2 ppa pla ppr plr d c
    T c -> liftPrettyPrec2 ppa pla ppr plr d c
    Sep -> showChar ';'

instance Pretty2 DefinitionConstraint where
  liftPrettyPrec2 _ _ ppr _ d (name := def) = showParen (d > 9) $ prettyPrec 0 name . showString " := " . maybe (showChar '_') (ppr 10) def

instance Pretty2 TypeConstraint where
  liftPrettyPrec2 _ _ ppr _ d (name ::: ty) = showParen (d > 9) $ prettyPrec 10 name . showString " :: " . ppr 10 ty
