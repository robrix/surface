{-# LANGUAGE GADTs #-}
module Judgement where

import Control.Monad
import Control.Monad.Free.Freer
import Data.Functor.Classes
import Data.Functor.Foldable
import Expr
import Text.Pretty

data JudgementF a where
  Check :: Term -> Type -> JudgementF ()
  Infer :: Term -> JudgementF Type

  IsType :: Term -> JudgementF ()

  Fresh :: JudgementF Type

data JudgementError = Expected Type Type

type Judgement = Freer JudgementF

infer :: Term -> Judgement Type
infer = liftF . Infer

check :: Term -> Type -> Judgement ()
check = (liftF .) . Check

isType :: Term -> Judgement ()
isType = liftF . IsType

fresh :: Judgement Type
fresh = liftF Fresh


decompose :: JudgementF a -> Judgement a
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

instance Show1 JudgementF where
  liftShowsPrec _ _ d judgement = case judgement of
    Check term ty -> showsBinaryWith showsPrec showsPrec "Check" d term ty
    Infer term -> showsUnaryWith showsPrec "Infer" d term

    IsType ty -> showsUnaryWith showsPrec "IsType" d ty

    Fresh -> showString "Fresh"
