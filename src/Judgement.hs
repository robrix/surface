{-# LANGUAGE GADTs, RankNTypes #-}
module Judgement where

import Control.Monad
import qualified Control.Monad.Fail as Fail
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Result
import Expr
import Text.Pretty

data Judgement a where
  Check :: Term -> Type -> Judgement ()
  Infer :: Term -> Judgement Type

  IsType :: Term -> Judgement ()

  Fresh :: Judgement Type

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
      return (a .*. b)

    Fst p -> do
      ty <- infer p
      case unfix ty of
        Product a _ -> return a
        _ -> fail ("Expected a product type, but got " ++ pretty ty)

    Snd p -> do
      ty <- infer p
      case unfix ty of
        Product _ b -> return b
        _ -> fail ("Expected a product type, but got " ++ pretty ty)

    InL l -> do
      a <- infer l
      b <- fresh
      return (a .+. b)

    InR r -> do
      a <- fresh
      b <- infer r
      return (a .+. b)

    Case subject ifL ifR -> do
      ty <- infer subject
      case unfix ty of
        Sum l r -> do
          b <- fresh
          check (l .->. b) ifL
          check (r .->. b) ifR
          return b
        _ -> fail ("Expected a sum type, but got " ++ pretty ty)

    Unit -> return unitT

    -- Types
    UnitT -> return typeT
    TypeT -> return typeT -- Impredicativity.
    Function{} -> isType term >> return typeT
    Product{} -> isType term >> return typeT
    Sum{} -> isType term >> return typeT

    _ -> fail ("No rule to infer type of " ++ pretty term)

  Check term ty -> do
    ty' <- infer term
    unless (ty' == ty) $ fail ("expected " ++ pretty ty ++ " but got " ++ pretty ty')

  IsType ty -> case unfix ty of
    UnitT -> return ()
    TypeT -> return ()
    Sum a b -> do
      isType a
      isType b
    Product a b -> do
      isType a
      isType b
    Function a b -> do
      isType a
      isType b
    _ -> fail ("Expected a Type but got " ++ pretty ty)

  Fresh -> return (Fix (Var (Name (-1))))


iterGoal :: (forall x. f x -> (x -> Result a) -> Result a) -> Goal f a -> Result a
iterGoal algebra = go
  where go goal = case goal of
          Failure s -> Error s
          Return a -> Result a
          Then instruction cont -> algebra instruction (go . cont)

interpret :: Goal Judgement a -> Result a
interpret = iterGoal $ \ judgement cont ->
  interpret (decompose judgement) >>= cont


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

instance Show1 f => Show1 (Goal f) where
  liftShowsPrec sp sa d goal = case goal of
    Failure es -> showsUnaryWith showsPrec "Failure" d es
    Return a -> showsUnaryWith sp "Return" d a
    Then inst cont -> showsBinaryWith (liftShowsPrec (\ i -> liftShowsPrec sp sa i . cont) (liftShowList sp sa . fmap cont)) (const showString) "Then" d inst "id"

instance (Show1 f, Show a) => Show (Goal f a) where
  showsPrec = liftShowsPrec showsPrec showList
