{-# LANGUAGE DeriveFoldable, DeriveFunctor, GADTs #-}
module Expr where

import Data.Functor.Foldable
import Text.Pretty

data ExprF a where
  Product :: a -> a -> ExprF a
  Sum :: a -> a -> ExprF a
  Function :: a -> a -> ExprF a
  UnitT :: ExprF a
  TypeT :: ExprF a

  Abs :: Name -> a -> ExprF a
  Var :: Name -> ExprF a
  App :: a -> a -> ExprF a

  InL :: a -> ExprF a
  InR :: a -> ExprF a
  Case :: a -> a -> a -> ExprF a

  Pair :: a -> a -> ExprF a
  Fst :: a -> ExprF a
  Snd :: a -> ExprF a

  Unit :: ExprF a
  deriving (Eq, Foldable, Functor, Show)

type Expr = Fix ExprF

type TypeF = ExprF
type Type = Fix TypeF

type TermF = ExprF
type Term = Fix TermF


newtype Name = Name Integer
  deriving (Eq, Show)

unName :: Name -> Integer
unName (Name n) = n


unitT :: Type
unitT = Fix UnitT

typeT :: Type
typeT = Fix TypeT

boolT :: Type
boolT = unitT .+. unitT

maybeT :: Type -> Type
maybeT = (unitT .+.)

eitherT :: Type -> Type -> Type
eitherT = (.+.)


infixr 0 .->.
(.->.) :: Type -> Type -> Type
a .->. b = Fix (Function a b)

infixl 6 .+.
(.+.) :: Type -> Type -> Type
(.+.) = (Fix .) . Sum

infixl 7 .*.
(.*.) :: Type -> Type -> Type
(.*.) = (Fix .) . Product

lam :: (Term -> Term) -> Term
lam f = Fix (Abs n body)
  where body = f (Fix (Var n))
        n = Name (succ (maxBoundVariable body))
        maxBoundVariable = cata $ \ term -> case term of
          App o a -> max o a
          Abs (Name v) _ -> v
          _ -> -1


infixr 0 #
(#) :: Term -> Term -> Term
a # b = Fix (App a b)


inL :: Term -> Term
inL = Fix . InL

inR :: Term -> Term
inR = Fix . InR

case' :: Term -> (Term -> Term) -> (Term -> Term) -> Term
case' t f g = Fix (Case t (lam f) (lam g))

pair :: Term -> Term -> Term
pair = (Fix .) . Pair

fst' :: Term -> Term
fst' = Fix . Fst

snd' :: Term -> Term
snd' = Fix . Snd

unit :: Term
unit = Fix Unit


-- Instances

instance Pretty1 ExprF where
  prettyPrec1 term = case term of
    App a b -> (0, prettyParen 0 a . showString " # " . prettyParen 0 b)
    Abs v b -> (10, showString "lam " . showParen True (showChar '\\' . showName v . showString " -> " . snd b))
    Var v -> (-1, showName v)
    InL l -> (10, showString "inL" . showChar ' ' . prettyParen 10 l)
    InR r -> (10, showString "inR" . showChar ' ' . prettyParen 10 r)
    Case c l r -> (10, showString "case " . prettyParen 10 c . showChar ' ' . prettyParen 10 l . showChar ' ' . prettyParen 10 r)
    Pair a b -> (10, showString "pair " . prettyParen 10 a . showChar ' ' . prettyParen 10 b)
    Fst f -> (10, showString "fst' " . prettyParen 10 f)
    Snd s -> (10, showString "snd'" . prettyParen 10 s)
    Function a b -> (0, prettyParen 0 a . showString " .->. " . prettyParen 0 b)
    Sum a b -> (6, prettyParen 6 a . showString " .+. " . prettyParen 7 b)
    Product a b -> (7, prettyParen 7 a . showString " .*. " . prettyParen 8 b)
    UnitT -> (-1, showString "unitT")
    Unit -> (-1, showString "unit")
    TypeT -> (-1, showString "typeT")
    where showName = showChar . ("abcdefghijklmnopqrstuvwxyz" !!) . fromInteger . unName
