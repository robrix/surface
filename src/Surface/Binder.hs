{-# LANGUAGE FlexibleInstances #-}
module Surface.Binder where

import Context
import Data.Functor.Foldable
import Expr

class Binder a where
  (<?) :: Name -> a -> Bool

class Binder1 f where
  liftIn :: (Name -> a -> Bool) -> Name -> f a -> Bool

instance (Foldable t, Binder a) => Binder (t a) where
  (<?) name = any (name <?)

instance Binder Name where
  (<?) = (==)

instance Binder DefinitionConstraint where
  name <? (_ := m) = name <? m

instance Binder TypeConstraint where
  name <? (_ ::: s) = name <? s

instance Binder1 f => Binder (Fix f) where
   (<?) name = liftIn (<?) name . unfix

instance Binder1 (ExprF Name) where
  liftIn occurs name expr = case expr of
    Abs n _ | n == name -> False
    Var v | v == name -> True
    _ -> any (occurs name) expr

instance Binder Constraint where
  n <? t = case t of
    Ty d -> n <? d
    Tm d -> n <? d
    Sep -> False
