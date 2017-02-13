{-# LANGUAGE DeriveFoldable, DeriveFunctor, GADTs #-}
module Expr where

import Data.Functor.Classes
import Data.Functor.Foldable
import Data.List (nub, sort, union)
import Data.Semigroup (Semigroup, Max(..), Option(..))
import Text.Pretty

data ExprF a where
  Product :: a -> a -> ExprF a
  Sum :: a -> a -> ExprF a
  Function :: a -> a -> ExprF a
  Pi :: Name -> a -> a -> ExprF a
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

  Let :: Name -> a -> a -> ExprF a

  As :: a -> a -> ExprF a
  deriving (Eq, Foldable, Functor, Show)

type Expr = Fix ExprF

type TypeF = ExprF
type Type = Fix TypeF

type TermF = ExprF
type Term = Fix TermF


data Name = N String
          | I Integer
  deriving (Eq, Ord, Show)


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
lam = uncurry makeLambda . bindVariable

makeLambda :: Name -> Term -> Term
makeLambda = (Fix .) . Abs

bindVariable :: (Term -> Term) -> (Name, Term)
bindVariable f = (n, body)
  where body = f (var n)
        n = I (succ (maxBoundVariable body))
        maxBoundVariable = cata $ \ term -> case term of
          App o a -> max o a
          Abs (I v) _ -> v
          _ -> -1

var :: Name -> Expr
var = Fix . Var

varN :: String -> Expr
varN = var . N

varI :: Integer -> Expr
varI = var . I


infixl 9 #
(#) :: Term -> Term -> Term
a # b = Fix (App a b)


inL :: Term -> Term
inL = Fix . InL

inR :: Term -> Term
inR = Fix . InR

case' :: Term -> (Term -> Term) -> (Term -> Term) -> Term
case' t f g = makeCase t (lam f) (lam g)

makeCase :: Term -> Term -> Term -> Term
makeCase t l r = Fix (Case t l r)

pair :: Term -> Term -> Term
pair = (Fix .) . Pair

fst' :: Term -> Term
fst' = Fix . Fst

snd' :: Term -> Term
snd' = Fix . Snd

unit :: Term
unit = Fix Unit

let' :: Term -> (Term -> Term) -> Term
let' value = uncurry (`makeLet` value) . bindVariable

makeLet :: Name -> Term -> Term -> Term
makeLet name value body = Fix (Let name value body)

as :: Term -> Type -> Term
as = (Fix .) . As

piT :: Type -> (Type -> Type) -> Type
piT ty = uncurry (`makePi` ty) . bindVariable

makePi :: Name -> Type -> Type -> Type
makePi name ty body = Fix (Pi name ty body)


-- Substitution

rename :: Name -> Name -> Expr -> Expr
rename from to = para $ \ expr -> case expr of
  Var v | v == from -> var to
  Abs v (original, substituted) -> makeLambda v (if v == from then original else substituted)
  _ -> Fix (fmap snd expr)

freeVariables :: Expr -> [Name]
freeVariables = nub . sort . cata alg
  where alg expr = case expr of
          Pi n t b -> t ++ filter (/=n) b
          Abs n b -> filter (/=n) b
          Var v -> [v]
          _ -> concat expr

freshNameIn :: [Name] -> Name
freshNameIn = maybe (I 0) (succ' . getMax) . sfoldMap Max
  where succ' (I i) = I (succ i)
        succ' (N n) = N (n ++ "'")

-- | Capture-avoiding substitution of an Expr for variables with a given Name in an Expr.
substitute :: Expr -> Name -> Expr -> Expr
substitute to from = para $ \ expr -> case expr of
  Var name
    | name == from -> to
    | otherwise    -> var name
  Abs name (original, substituted)
    | name == from -> let fresh = freshNameIn (freeVariables original `union` freeVariables to) in
                      Fix (Abs fresh (substitute to from (rename name fresh original)))
    | otherwise    -> Fix (Abs name substituted)
  _ -> Fix (fmap snd expr)


-- Conveniences

sfoldMap :: (Semigroup s, Foldable t) => (a -> s) -> t a -> Maybe s
sfoldMap f = getOption . foldMap (Option . Just . f)


-- Instances

instance Pretty1 ExprF where
  liftPrettyPrec pp d expr = case expr of
    App a b -> showParen (d > 10) $ pp 10 a . showChar ' ' . pp 11 b
    Abs v b -> showParen (d > 0) $ showChar '\\' . prettyPrec 0 v . showString " . " . pp 0 b
    Var v -> prettyPrec 0 v
    InL l -> showParen (d > 10) $ showString "inL " . pp 11 l
    InR r -> showParen (d > 10) $ showString "inR " . pp 11 r
    Case c l r -> showParen (d > 10) $ showString "case " . pp 0 c . showString " of " . pp 11 l . showChar ' ' . pp 11 r
    Pair a b -> showParen (d >= 0) $ pp 0 a . showString ", " . pp (negate 1) b
    Fst f -> showParen (d > 10) $ showString "fst " . pp 11 f
    Snd s -> showParen (d > 10) $ showString "snd " . pp 11 s
    Function a b -> showParen (d > 0) $ pp 1 a . showString " -> " . pp 0 b
    Pi n t b -> showParen (d > 0) $ showParen True (prettyPrec 0 n . showString " : " . pp 1 t) . showString " -> " . pp 0 b
    Sum a b -> showParen (d > 6) $ pp 6 a . showString " + " . pp 7 b
    Product a b -> showParen (d > 7) $ pp 7 a . showString " * " . pp 8 b
    UnitT -> showString "Unit"
    Unit -> showString "()"
    TypeT -> showString "Type"
    Let n v b -> showParen (d > 10) $ showString "let " . prettyPrec 0 n . showString " = " . pp 0 v . showString " in " . pp 0 b
    As term ty -> showParen (d > 0) $ pp 1 term . showString " : " . pp 0 ty

instance Pretty Name where
  prettyPrec _ name = case name of
    N s -> showString s
    I i -> showChar '_' . shows i

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
    (Pi n1 t1 b1, Pi n2 t2 b2) -> n1 == n2 && eq t1 t2 && eq b1 b2
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
    Pi n t b -> showsTernaryWith showsPrec sp sp "Pi" d n t b
    Sum a b -> showsBinaryWith sp sp "Sum" d a b
    Product a b -> showsBinaryWith sp sp "Product" d a b
    UnitT -> showString "UnitT"
    Unit -> showString "Unit"
    TypeT -> showString "TypeT"
    Let n v b -> showsTernaryWith showsPrec sp sp "Let" d n v b
    As term ty -> showsBinaryWith sp sp "As" d term ty

showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $
  showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z
