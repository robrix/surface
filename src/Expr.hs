{-# LANGUAGE DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, GADTs #-}
module Expr where

import Data.Bifoldable
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Foldable hiding (Mu)
import Data.Hashable (Hashable)
import Data.List (nub, sort, union)
import Data.Semigroup (Semigroup(..), Max(..), Option(..))
import GHC.Generics (Generic)
import Text.Pretty

data ExprF n a where
  Product :: a -> a -> ExprF n a
  Sum :: a -> a -> ExprF n a
  Pi :: n -> a -> a -> ExprF n a
  Mu :: n -> a -> a -> ExprF n a
  Sigma :: n -> a -> a -> ExprF n a

  UnitT :: ExprF n a
  Type :: ExprF n a

  Abs :: n -> a -> ExprF n a
  Var :: n -> ExprF n a
  App :: a -> a -> ExprF n a

  InL :: a -> ExprF n a
  InR :: a -> ExprF n a
  Case :: a -> a -> a -> ExprF n a

  Pair :: a -> a -> ExprF n a
  Fst :: a -> ExprF n a
  Snd :: a -> ExprF n a

  Unit :: ExprF n a

  Let :: n -> a -> a -> ExprF n a

  As :: a -> a -> ExprF n a
  deriving (Eq, Foldable, Functor, Show, Traversable)

type Expr = Fix (ExprF Name)

type TypeF = ExprF
type Type = Fix (TypeF Name)

type TermF = ExprF
type Term = Fix (TermF Name)


data Name = N String
          | I Integer
  deriving (Eq, Generic, Hashable, Ord, Show)


unitT :: Type
unitT = Fix UnitT

typeT :: Type
typeT = Fix Type


infixr 0 .->.
(.->.) :: Type -> Type -> Type
(.->.) = makePi (I (negate 1))

infixr 6 .+.
(.+.) :: Type -> Type -> Type
(.+.) = (Fix .) . Sum

infixr 7 .*.
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
          Abs (I v) _ -> v
          Pi (I v) t _ -> max t v
          _ -> getMax (foldr ((<>) . Max) (Max (negate 1)) term)

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


mu :: Type -> (Type -> Type) -> Type
mu ty = uncurry (`makeMu` ty) . bindVariable

makeMu :: Name -> Type -> Type -> Type
makeMu name ty body = Fix (Mu name ty body)


sigmaT :: Type -> (Type -> Type) -> Type
sigmaT ty = uncurry (`makeSigma` ty) . bindVariable

makeSigma :: Name -> Type -> Type -> Type
makeSigma name ty body = Fix (Sigma name ty body)


-- Type elimination

asPi :: Expr -> Maybe (Name, Expr, Expr)
asPi expr = case unfix expr of
  Pi n ty body -> Just (n, ty, body)
  _ -> Nothing

asApplication :: Expr -> Maybe (Expr, Expr)
asApplication expr = case unfix expr of
  App a b -> Just (a, b)
  _ -> Nothing


domain :: Type -> [Type]
domain = maybe [] (\ (_, t, b) -> t : domain b) . asPi

codomain :: Type -> Type
codomain expr = maybe expr (\ (_, _, b) -> codomain b) (asPi expr)


applicationChain :: Expr -> (Expr, [Expr])
applicationChain = go . flip (,) []
  where go (expr, args) = maybe (expr, args) (go . second (: args)) (asApplication expr)


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
freshNameIn = maybe (I 0) (succName . getMax) . sfoldMap Max

succName :: Name -> Name
succName (I i) = I (succ i)
succName (N n) = N (n ++ "'")

generalize :: [Name] -> Expr -> Expr
generalize = flip (foldr makeLambda)

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


-- Traversal

zipExprFWith :: (m -> n -> o) -> (a -> b -> c) -> ExprF m a -> ExprF n b -> Maybe (ExprF o c)
zipExprFWith g f a b = case (a, b) of
  (Product a1 b1, Product a2 b2) -> Just (Product (f a1 a2) (f b1 b2))
  (Sum a1 b1, Sum a2 b2) -> Just (Sum (f a1 a2) (f b1 b2))
  (Pi n1 t1 b1, Pi n2 t2 b2) -> Just (Pi (g n1 n2) (f t1 t2) (f b1 b2))
  (Mu n1 t1 b1, Mu n2 t2 b2) -> Just (Mu (g n1 n2) (f t1 t2) (f b1 b2))
  (Sigma n1 t1 b1, Sigma n2 t2 b2) -> Just (Sigma (g n1 n2) (f t1 t2) (f b1 b2))

  (UnitT, UnitT) -> Just UnitT
  (Type, Type) -> Just Type

  (Abs n1 b1, Abs n2 b2) -> Just (Abs (g n1 n2) (f b1 b2))
  (Var n1, Var n2) -> Just (Var (g n1 n2))
  (App a1 b1, App a2 b2) -> Just (App (f a1 a2) (f b1 b2))

  (InL a1, InL a2) -> Just (InL (f a1 a2))
  (InR a1, InR a2) -> Just (InR (f a1 a2))
  (Case c1 l1 r1, Case c2 l2 r2) -> Just (Case (f c1 c2) (f l1 l2) (f r1 r2))

  (Pair a1 b1, Pair a2 b2) -> Just (Pair (f a1 a2) (f b1 b2))
  (Fst a1, Fst a2) -> Just (Fst (f a1 a2))
  (Snd a1, Snd a2) -> Just (Snd (f a1 a2))

  (Unit, Unit) -> Just Unit

  (Let n1 v1 b1, Let n2 v2 b2) -> Just (Let (g n1 n2) (f v1 v2) (f b1 b2))

  (As a1 b1, As a2 b2) -> Just (As (f a1 a2) (f b1 b2))

  _ -> Nothing


-- Conveniences

sfoldMap :: (Semigroup s, Foldable t) => (a -> s) -> t a -> Maybe s
sfoldMap f = getOption . foldMap (Option . Just . f)


-- Instances

instance Bifunctor ExprF where
  bimap g f expr = case expr of
    Product a b -> Product (f a) (f b)
    Sum a b -> Sum (f a) (f b)
    Pi n t b -> Pi (g n) (f t) (f b)
    Mu n t b -> Mu (g n) (f t) (f b)
    Sigma n t b -> Sigma (g n) (f t) (f b)

    UnitT -> UnitT
    Type -> Type

    Abs n b -> Abs (g n) (f b)
    Var n -> Var (g n)
    App a b -> App (f a) (f b)

    InL a -> InL (f a)
    InR a -> InR (f a)
    Case c l r -> Case (f c) (f l) (f r)

    Pair a b -> Pair (f a) (f b)
    Fst a -> Fst (f a)
    Snd a -> Snd (f a)

    Unit -> Unit

    Let n v b -> Let (g n) (f v) (f b)

    As a b -> As (f a) (f b)

instance Bifoldable ExprF where
  bifoldMap g f expr = case expr of
    Product a b -> mappend (f a) (f b)
    Sum a b -> mappend (f a) (f b)
    Pi n t b -> mappend (g n) (mappend (f t) (f b))
    Mu n t b -> mappend (g n) (mappend (f t) (f b))
    Sigma n t b -> mappend (g n) (mappend (f t) (f b))

    UnitT -> mempty
    Type -> mempty

    Abs n b -> mappend (g n) (f b)
    Var n -> g n
    App a b -> mappend (f a) (f b)

    InL a -> f a
    InR a -> f a
    Case c l r -> mappend (f c) (mappend (f l) (f r))

    Pair a b -> mappend (f a) (f b)
    Fst a -> f a
    Snd a -> f a

    Unit -> mempty

    Let n v b -> mappend (g n) (mappend (f v) (f b))

    As a b -> mappend (f a) (f b)

instance Pretty2 ExprF where
  liftPrettyPrec2 pn _ pp _ d expr = case expr of
    App a b -> showParen (d > 10) $ pp 10 a . showChar ' ' . pp 11 b
    Abs v b -> showParen (d > 0) $ showChar '\\' . pn 0 v . showString " . " . pp 0 b
    Var v -> pn 0 v
    InL l -> showParen (d > 10) $ showString "inL " . pp 11 l
    InR r -> showParen (d > 10) $ showString "inR " . pp 11 r
    Case c l r -> showParen (d > 10) $ showString "case " . pp 0 c . showString " of " . pp 11 l . showChar ' ' . pp 11 r
    Pair a b -> showParen (d >= 0) $ pp 0 a . showString ", " . pp (negate 1) b
    Fst f -> showParen (d > 10) $ showString "fst " . pp 11 f
    Snd s -> showParen (d > 10) $ showString "snd " . pp 11 s
    Pi n t b -> showParen (d > 0) $ showParen True (pn 0 n . showString " : " . pp 1 t) . showString " -> " . pp 0 b
    Mu n t b -> showParen (d > 0) $ showString "µ " . pn 0 n . showString " : " . pp 1 t . showString " . " . pp 0 b
    Sigma n t b -> showBrace True $ pn 0 n . showString " : " . pp 1 t . showString " | " . pp 0 b
    Sum a b -> showParen (d > 6) $ pp 7 a . showString " + " . pp 6 b
    Product a b -> showParen (d > 7) $ pp 8 a . showString " * " . pp 7 b
    UnitT -> showString "Unit"
    Unit -> showString "()"
    Type -> showString "Type"
    Let n v b -> showParen (d > 10) $ showString "let " . pn 0 n . showString " = " . pp 0 v . showString " in " . pp 0 b
    As term ty -> showParen (d > 0) $ pp 1 term . showString " : " . pp 0 ty
    where showBrace b s = if b then showString "{ " . s . showString " }" else s

instance Pretty Name where
  prettyPrec _ name = case name of
    N s -> showString s
    I i | i >= 0 -> showChar '_' . shows i
        | otherwise -> showChar '_'

instance Eq n => Eq1 (ExprF n) where
  liftEq eq = (maybe False biand .) . zipExprFWith (==) eq

instance Show n => Show1 (ExprF n) where
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
    Pi n t b -> showsTernaryWith showsPrec sp sp "Pi" d n t b
    Mu n t b -> showsTernaryWith showsPrec sp sp "Mu" d n t b
    Sigma n t b -> showsTernaryWith showsPrec sp sp "Sigma" d n t b
    Sum a b -> showsBinaryWith sp sp "Sum" d a b
    Product a b -> showsBinaryWith sp sp "Product" d a b
    UnitT -> showString "UnitT"
    Unit -> showString "Unit"
    Type -> showString "Type"
    Let n v b -> showsTernaryWith showsPrec sp sp "Let" d n v b
    As term ty -> showsBinaryWith sp sp "As" d term ty

showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $
  showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z
