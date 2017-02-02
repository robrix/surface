{-# LANGUAGE DeriveFoldable, DeriveFunctor, GADTs #-}
module Expr where

import Data.Functor.Classes
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
  deriving (Eq, Ord, Show)

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
  where body = f (var n)
        n = Name (succ (maxBoundVariable body))
        maxBoundVariable = cata $ \ term -> case term of
          App o a -> max o a
          Abs (Name v) _ -> v
          _ -> -1

var :: Name -> Expr
var = Fix . Var


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
    Abs v b -> (10, showString "lam " . showParen True (showChar '\\' . snd (prettyPrec v) . showString " -> " . snd b))
    Var v -> prettyPrec v
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

instance Pretty Name where
  prettyPrec = (,) (negate 1) . showChar . ("abcdefghijklmnopqrstuvwxyz" !!) . fromInteger . unName

instance Eq1 ExprF where
  liftEq eq a b = case (a, b) of
    (App o1 a1, App o2 a2) -> eq o1 o2 && eq a1 a2
    (Abs v1 r1, Abs v2 r2) -> v1 == v2 && eq r1 r2
    (Var v1, Var v2) -> v1 == v2

    (InL l1, InL l2) -> eq l1 l2
    (InR r1, InR r2) -> eq r1 r2
    (Case c1 l1 r1, Case c2 l2 r2) -> eq c1 c2 && eq l1 l2 && eq r1 r2

    (Pair a1 b1, Pair a2 b2) -> eq a1 a2 && eq b1 b2
    (Fst p1, Fst p2) -> eq p1 p2
    (Snd p1, Snd p2) -> eq p1 p2

    (Unit, Unit) -> True

    (Function a1 b1, Function a2 b2) -> eq a1 a2 && eq b1 b2
    (Sum a1 b1, Sum a2 b2) -> eq a1 a2 && eq b1 b2
    (Product a1 b1, Product a2 b2) -> eq a1 a2 && eq b1 b2
    (UnitT, UnitT) -> True
    (TypeT, TypeT) -> True
    _ -> False

instance Show1 ExprF where
  liftShowsPrec sp _ d t = case t of
    App a b -> showsBinaryWith sp sp "App" d a b
    Abs v b -> showsBinaryWith showsPrec sp "Abs" d v b
    Var v -> showsUnaryWith showsPrec "Var" d v
    InL l -> showsUnaryWith sp "InL" d l
    InR r -> showsUnaryWith sp "InR" d r
    Case c l r -> showsTernaryWith sp sp sp "Case" d c l r
    Pair a b -> showsBinaryWith sp sp "Pair" d a b
    Fst f -> showsUnaryWith sp "Fst" d f
    Snd s -> showsUnaryWith sp "Snd" d s
    Function a b -> showsBinaryWith sp sp "Function" d a b
    Sum a b -> showsBinaryWith sp sp "Sum" d a b
    Product a b -> showsBinaryWith sp sp "Product" d a b
    UnitT -> showString "UnitT"
    Unit -> showString "Unit"
    TypeT -> showString "TypeT"
    where
      showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
      showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $
        showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z
