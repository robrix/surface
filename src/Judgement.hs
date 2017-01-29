{-# LANGUAGE GADTs #-}
module Judgement where

import Control.Monad.Free.Freer
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
