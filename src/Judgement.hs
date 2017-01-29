{-# LANGUAGE GADTs #-}
module Judgement where

import Expr

data Judgement a where
  Check :: Term -> Type -> Judgement ()
  Infer :: Term -> Judgement Type

  IsType :: Term -> Judgement ()

  Hole :: Judgement Type
