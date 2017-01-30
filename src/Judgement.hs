{-# LANGUAGE GADTs #-}
module Judgement where

import Control.Monad
import qualified Control.Monad.Fail as Fail
import Data.Functor.Classes
import Data.Functor.Foldable
import Expr
import Text.Pretty

data Judgement a where
  Check :: Term -> Type -> Judgement ()
  Infer :: Term -> Judgement Type

  IsType :: Term -> Judgement ()

  Fresh :: Judgement Type

data JudgementError = Expected Type Type

data Goal f a where
  Failure :: [String] -> Goal f a
  Return :: a -> Goal f a
  Then :: f x -> (x -> Goal f a) -> Goal f a



infer :: Term -> Goal Judgement Type
infer term = Infer term `Then` Return

check :: Term -> Type -> Goal Judgement ()
check term ty = Check term ty `Then` Return

isType :: Term -> Goal Judgement ()
isType term = IsType term `Then` Return

fresh :: Goal Judgement Type
fresh = Fresh `Then` Return


decompose :: Judgement a -> Goal Judgement a
decompose judgement = case judgement of
  Infer term -> case unfix term of
    Pair x y -> do
      a <- infer x
      b <- infer y
      pure (a .*. b)

    Fst p -> do
      Fix (Product a _) <- infer p
      pure a

    Snd p -> do
      Fix (Product _ b) <- infer p
      pure b

    InL l -> do
      a <- infer l
      b <- fresh
      pure (a .+. b)

    InR r -> do
      a <- fresh
      b <- infer r
      pure (a .+. b)

    Case subject ifL ifR -> do
      Fix (Sum l r) <- infer subject
      b <- fresh
      check (l .->. b) ifL
      check (r .->. b) ifR
      pure b

    Unit ->
      pure unitT

    _ -> do
      isType term
      pure typeT

  Check term ty -> do
    ty' <- infer term
    unless (ty' == ty) $ fail ("expected " ++ pretty ty ++ " but got " ++ pretty ty')

  IsType ty -> case unfix ty of
    UnitT -> pure ()
    TypeT -> pure ()
    Sum a b -> do
      isType a
      isType b
    Product a b -> do
      isType a
      isType b
    Function a b -> do
      isType a
      isType b
    _ -> pure () -- Is this correct…?

  Fresh -> pure (Fix (Var (Name (-1))))


-- Instances

instance Show1 Judgement where
  liftShowsPrec _ _ d judgement = case judgement of
    Check term ty -> showsBinaryWith showsPrec showsPrec "Check" d term ty
    Infer term -> showsUnaryWith showsPrec "Infer" d term

    IsType ty -> showsUnaryWith showsPrec "IsType" d ty

    Fresh -> showString "Fresh"

instance Functor (Goal f) where
  fmap f g = case g of
    Failure s -> Failure s
    Return a -> Return (f a)
    Then r t -> Then r (fmap f . t)

instance Applicative (Goal f) where
  pure = Return
  Failure s <*> _ = Failure s
  Return f <*> a = fmap f a
  Then instruction cont <*> a = instruction `Then` ((<*> a) . cont)

instance Monad (Goal f) where
  return = pure
  fail = Fail.fail
  Failure s >>= _ = Failure s
  Return a >>= f = f a
  Then instruction cont >>= f = instruction `Then` ((>>= f) . cont)

instance Fail.MonadFail (Goal f) where
  fail = Failure . pure
