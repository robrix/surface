{-# LANGUAGE GADTs #-}
module Judgement where

import Context
import Data.Functor.Classes
import Expr
import Module
import Prelude hiding (fail)
import Text.Pretty

data Judgement a where
  CheckModule :: Module -> Judgement ()
  CheckDeclaration :: Module -> Declaration -> Judgement ()

  Check :: Term -> Type -> Judgement ()
  Infer :: Term -> Judgement Type
  IsType :: Term -> Judgement ()

  AlphaEquivalent :: Expr -> Expr -> Judgement Bool
  Equate :: Expr -> Expr -> Judgement ()

  Unify :: Type -> Type -> Judgement ()
  Solve :: Name -> Suffix -> Type -> Judgement ()

  Fresh :: Maybe Expr -> Judgement Name
  Restore :: Judgement Extension
  Replace :: Suffix -> Judgement Extension

  Normalize :: Expr -> Judgement Expr
  WHNF :: Expr -> Judgement Expr


-- Instances

instance Show1 Judgement where
  liftShowsPrec _ _ d judgement = case judgement of
    CheckModule module' -> showsUnaryWith showsPrec "CheckModule" d module'
    CheckDeclaration module' declaration -> showsBinaryWith showsPrec showsPrec "CheckDeclaration" d module' declaration

    Check term ty -> showsBinaryWith showsPrec showsPrec "Check" d term ty
    Infer term -> showsUnaryWith showsPrec "Infer" d term

    IsType ty -> showsUnaryWith showsPrec "IsType" d ty

    AlphaEquivalent e1 e2 -> showsBinaryWith showsPrec showsPrec "AlphaEquivalent" d e1 e2
    Equate e1 e2 -> showsBinaryWith showsPrec showsPrec "Equate" d e1 e2

    Unify t1 t2 -> showsBinaryWith showsPrec showsPrec "Unify" d t1 t2
    Solve name suffix ty -> showsTernaryWith showsPrec showsPrec showsPrec "Solve" d name suffix ty

    Fresh declaration -> showsUnaryWith showsPrec "Fresh" d declaration
    Judgement.Restore -> showString "Restore"
    Judgement.Replace suffix -> showsUnaryWith showsPrec "Replace" d suffix

    Normalize expr -> showsUnaryWith showsPrec "Normalize" d expr
    WHNF expr -> showsUnaryWith showsPrec "WHNF" d expr

instance Show a => Show (Judgement a) where
  showsPrec = showsPrec1

instance Pretty1 Judgement where
  liftPrettyPrec _ _ d judgement = case judgement of
    CheckModule (Module name _) -> showsUnaryWith (const showString) "checkModule" d name
    CheckDeclaration (Module modName _) decl -> showsUnaryWith (const showString) "checkDeclaration" d (modName ++ "." ++ pretty (declarationName decl))

    Check term ty -> showsBinaryWith prettyPrec prettyPrec "check" d term ty
    Infer term -> showsUnaryWith prettyPrec "infer" d term
    IsType ty -> showsUnaryWith prettyPrec "isType" d ty

    AlphaEquivalent e1 e2 -> showsBinaryWith prettyPrec prettyPrec "alphaEquivalent" d e1 e2
    Equate e1 e2 -> showsBinaryWith prettyPrec prettyPrec "equate" d e1 e2

    Unify t1 t2 -> showsBinaryWith prettyPrec prettyPrec "unify" d t1 t2
    Solve n s ty -> showsTernaryWith prettyPrec prettyPrec prettyPrec "solve" d n s ty

    Fresh declaration -> showsUnaryWith (maybe (showString "_") . prettyPrec) "fresh" d declaration
    Judgement.Restore -> showString "restore"
    Judgement.Replace suffix -> showsUnaryWith prettyPrec "replace" d suffix

    Normalize expr -> showsUnaryWith prettyPrec "normalize" d expr
    WHNF expr -> showsUnaryWith prettyPrec "whnf" d expr

instance Eq1 Judgement where
  liftEq _ a b = case (a, b) of
    (CheckModule m1, CheckModule m2) -> m1 == m2
    (CheckDeclaration m1 d1, CheckDeclaration m2 d2) -> m1 == m2 && d1 == d2

    (Check tm1 ty1, Check tm2 ty2) -> tm1 == tm2 && ty1 == ty2
    (Infer tm1, Infer tm2) -> tm1 == tm2
    (IsType tm1, IsType tm2) -> tm1 == tm2

    (AlphaEquivalent a1 b1, AlphaEquivalent a2 b2) -> a1 == a2 && b1 == b2
    (Equate a1 b1, Equate a2 b2) -> a1 == a2 && b1 == b2

    (Unify a1 b1, Unify a2 b2) -> a1 == a2 && b1 == b2
    (Solve n1 s1 t1, Solve n2 s2 t2) -> n1 == n2 && s1 == s2 && t1 == t2

    (Fresh a1, Fresh a2) -> a1 == a2
    (Judgement.Restore, Judgement.Restore) -> True
    (Judgement.Replace s1, Judgement.Replace s2) -> s1 == s2

    (Normalize tm1, Normalize tm2) -> tm1 == tm2
    (WHNF tm1, WHNF tm2) -> tm1 == tm2

    _ -> False

instance Eq (Judgement a) where
  (==) = liftEq (const (const False))
