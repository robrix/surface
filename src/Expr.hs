{-# LANGUAGE DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, GADTs #-}
module Expr where

import Data.Bifoldable
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Foldable hiding (Mu)
import Data.Hashable (Hashable)
import Data.List (intersperse, nub, sort, union)
import Data.Semigroup (Semigroup(..), Max(..), Option(..))
import GHC.Generics (Generic)
import Text.Pretty

data ExprF n a where
  Product :: [a] -> ExprF n a
  Sum :: [a] -> ExprF n a
  Pi :: n -> a -> a -> ExprF n a
  Mu :: n -> a -> a -> ExprF n a
  Sigma :: n -> a -> a -> ExprF n a

  Type :: ExprF n a

  Abs :: n -> a -> ExprF n a
  Var :: n -> ExprF n a
  App :: a -> a -> ExprF n a

  InL :: a -> ExprF n a
  InR :: a -> ExprF n a
  Case :: a -> a -> a -> ExprF n a

  Tuple :: [a] -> ExprF n a
  Fst :: a -> ExprF n a
  Snd :: a -> ExprF n a

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
unitT = Fix (Product [])

voidT :: Type
voidT = Fix (Sum [])

typeT :: Type
typeT = Fix Type


infixr 0 .->.
(.->.) :: Type -> Type -> Type
(.->.) = makePi (I (negate 1))

infixr 6 .+.
(.+.) :: Type -> Type -> Type
a .+. Fix (Sum b) = Fix (Sum (a : b))
a .+. b = Fix (Sum [a, b])

sumT :: [Type] -> Type
sumT = Fix . Sum

infixr 7 .*.
(.*.) :: Type -> Type -> Type
a .*. Fix (Product b) = Fix (Product (a : b))
a .*. b = Fix (Product [a, b])

productT :: [Type] -> Type
productT = Fix . Product

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
a `pair` Fix (Tuple as) = Fix (Tuple (a : as))
a `pair` b = Fix (Tuple [a, b])

tuple :: [Term] -> Term
tuple = Fix . Tuple

fst' :: Term -> Term
fst' = Fix . Fst

snd' :: Term -> Term
snd' = Fix . Snd

unit :: Term
unit = Fix (Tuple [])

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
  (Product vs1, Product vs2) | length vs1 == length vs2 -> Just (Product (zipWith f vs1 vs2))
  (Sum vs1, Sum vs2) | length vs1 == length vs2 -> Just (Sum (zipWith f vs1 vs2))
  (Pi n1 t1 b1, Pi n2 t2 b2) -> Just (Pi (g n1 n2) (f t1 t2) (f b1 b2))
  (Mu n1 t1 b1, Mu n2 t2 b2) -> Just (Mu (g n1 n2) (f t1 t2) (f b1 b2))
  (Sigma n1 t1 b1, Sigma n2 t2 b2) -> Just (Sigma (g n1 n2) (f t1 t2) (f b1 b2))

  (Type, Type) -> Just Type

  (Abs n1 b1, Abs n2 b2) -> Just (Abs (g n1 n2) (f b1 b2))
  (Var n1, Var n2) -> Just (Var (g n1 n2))
  (App a1 b1, App a2 b2) -> Just (App (f a1 a2) (f b1 b2))

  (InL a1, InL a2) -> Just (InL (f a1 a2))
  (InR a1, InR a2) -> Just (InR (f a1 a2))
  (Case c1 l1 r1, Case c2 l2 r2) -> Just (Case (f c1 c2) (f l1 l2) (f r1 r2))

  (Tuple vs1, Tuple vs2) | length vs1 == length vs2 -> Just (Tuple (zipWith f vs1 vs2))
  (Fst a1, Fst a2) -> Just (Fst (f a1 a2))
  (Snd a1, Snd a2) -> Just (Snd (f a1 a2))

  (Let n1 v1 b1, Let n2 v2 b2) -> Just (Let (g n1 n2) (f v1 v2) (f b1 b2))

  (As a1 b1, As a2 b2) -> Just (As (f a1 a2) (f b1 b2))

  _ -> Nothing


-- Conveniences

sfoldMap :: (Semigroup s, Foldable t) => (a -> s) -> t a -> Maybe s
sfoldMap f = getOption . foldMap (Option . Just . f)


-- Instances

instance Bifunctor ExprF where
  bimap g f expr = case expr of
    Product vs -> Product (map f vs)
    Sum vs -> Sum (map f vs)
    Pi n t b -> Pi (g n) (f t) (f b)
    Mu n t b -> Mu (g n) (f t) (f b)
    Sigma n t b -> Sigma (g n) (f t) (f b)

    Type -> Type

    Abs n b -> Abs (g n) (f b)
    Var n -> Var (g n)
    App a b -> App (f a) (f b)

    InL a -> InL (f a)
    InR a -> InR (f a)
    Case c l r -> Case (f c) (f l) (f r)

    Tuple vs -> Tuple (map f vs)
    Fst a -> Fst (f a)
    Snd a -> Snd (f a)

    Let n v b -> Let (g n) (f v) (f b)

    As a b -> As (f a) (f b)

instance Bifoldable ExprF where
  bifoldMap g f expr = case expr of
    Product vs -> foldMap f vs
    Sum vs -> foldMap f vs
    Pi n t b -> mappend (g n) (mappend (f t) (f b))
    Mu n t b -> mappend (g n) (mappend (f t) (f b))
    Sigma n t b -> mappend (g n) (mappend (f t) (f b))

    Type -> mempty

    Abs n b -> mappend (g n) (f b)
    Var n -> g n
    App a b -> mappend (f a) (f b)

    InL a -> f a
    InR a -> f a
    Case c l r -> mappend (f c) (mappend (f l) (f r))

    Tuple vs -> foldMap f vs
    Fst a -> f a
    Snd a -> f a

    Let n v b -> mappend (g n) (mappend (f v) (f b))

    As a b -> mappend (f a) (f b)

instance Pretty2 ExprF where
  liftPrettyPrec2 pn _ pp _ d expr = case expr of
    Product [] -> showString "Unit"
    Product vs -> showParen (d > 7) $ foldr (.) id (intersperse (showString " * ") (map (pp 8) vs))
    Sum [] -> showString "Void"
    Sum vs -> showParen (d > 6) $ foldr (.) id (intersperse (showString " + ") (map (pp 7) vs))
    Pi n t b -> showParen (d > 0) $ showParen True (pn 0 n . showString " : " . pp 1 t) . showString " -> " . pp 0 b
    Mu n t b -> showParen (d > 0) $ showString "µ " . pn 0 n . showString " : " . pp 1 t . showString " . " . pp 0 b
    Sigma n t b -> showBrace True $ pn 0 n . showString " : " . pp 1 t . showString " | " . pp 0 b

    Type -> showString "Type"

    Abs v b -> showParen (d > 0) $ showChar '\\' . pn 0 v . showString " . " . pp 0 b
    Var v -> pn 0 v
    App a b -> showParen (d > 10) $ pp 10 a . showChar ' ' . pp 11 b

    InL l -> showParen (d > 10) $ showString "inL " . pp 11 l
    InR r -> showParen (d > 10) $ showString "inR " . pp 11 r
    Case c l r -> showParen (d > 10) $ showString "case " . pp 0 c . showString " of " . pp 11 l . showChar ' ' . pp 11 r

    Tuple vs -> showParen True $ foldr (.) id (intersperse (showString ", ") (map (pp 0) vs))
    Fst f -> showParen (d > 10) $ showString "fst " . pp 11 f
    Snd s -> showParen (d > 10) $ showString "snd " . pp 11 s

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

instance Show2 ExprF where
  liftShowsPrec2 spn _ spr slr d t = case t of
    Product vs -> showsUnaryWith (liftShowsPrec spr slr) "Product" d vs
    Sum vs -> showsUnaryWith (liftShowsPrec spr slr) "Sum" d vs
    Pi n t b -> showsTernaryWith spn spr spr "Pi" d n t b
    Mu n t b -> showsTernaryWith spn spr spr "Mu" d n t b
    Sigma n t b -> showsTernaryWith spn spr spr "Sigma" d n t b

    Type -> showString "Type"

    Abs v b -> showsBinaryWith spn spr "Abs" d v b
    Var v -> showsUnaryWith spn "Var" d v
    App a b -> showsBinaryWith spr spr "App" d a b

    InL l -> showsUnaryWith spr "InL" d l
    InR r -> showsUnaryWith spr "InR" d r
    Case c l r -> showsTernaryWith spr spr spr "Case" d c l r

    Tuple as -> showsUnaryWith (liftShowsPrec spr slr) "Tuple" d as
    Fst f -> showsUnaryWith spr "Fst" d f
    Snd s -> showsUnaryWith spr "Snd" d s

    Let n v b -> showsTernaryWith spn spr spr "Let" d n v b

    As term ty -> showsBinaryWith spr spr "As" d term ty

instance Show n => Show1 (ExprF n) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $
  showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z
