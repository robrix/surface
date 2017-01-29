{-# LANGUAGE GADTs #-}
module Judgement where

import Control.Monad
import Control.Monad.Free.Freer
import Data.Functor.Classes
import Data.Functor.Foldable
import Expr

data Judgement a where
  Check :: Term -> Type -> Judgement ()
  Infer :: Term -> Judgement Type

  IsType :: Term -> Judgement ()

  Hole :: Judgement Type

data JudgementError = Expected Type Type


infer :: Term -> Freer Judgement Type
infer = liftF . Infer

check :: Term -> Type -> Freer Judgement ()
check = (liftF .) . Check

isType :: Term -> Freer Judgement ()
isType = liftF . IsType

hole :: Freer Judgement Type
hole = liftF Hole


decompose :: Judgement a -> Freer Judgement a
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
      b <- hole
      pure (a .+. b)

    InR r -> do
      a <- hole
      b <- infer r
      pure (a .+. b)

    Unit ->
      pure unitT

    _ -> do
      isType term
      pure typeT

  Check term ty -> do
    ty' <- infer term
    unless (ty' == ty) (pure ())

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

  Hole -> pure (Fix (Var (Name (-1))))


-- Instances

instance Show1 Judgement where
  liftShowsPrec _ _ d judgement = case judgement of
    Check term ty -> showsBinaryWith showsPrec showsPrec "Check" d term ty
    Infer term -> showsUnaryWith showsPrec "Infer" d term

    IsType ty -> showsUnaryWith showsPrec "IsType" d ty

    Hole -> showString "Hole"
