{-# LANGUAGE DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable #-}
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

data ExprF n a
  = Product [a]
  | Sum [a]
  | Pi n a a
  | Mu n a a
  | Sigma n a a

  | Type

  | Abs n a
  | Var n
  | App a a

  | Inj a Int
  | Case a [a]

  | Tuple [a]
  | At a Int

  | Let n a a

  | As a a
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

inj :: Term -> Int -> Term
inj a i = Fix (Inj a i)

case' :: Term -> [(Term -> Term)] -> Term
case' t fs = makeCase t (lam <$> fs)

makeCase :: Term -> [Term] -> Term
makeCase = (Fix .) . Case

pair :: Term -> Term -> Term
a `pair` Fix (Tuple as) = Fix (Tuple (a : as))
a `pair` b = Fix (Tuple [a, b])

tuple :: [Term] -> Term
tuple = Fix . Tuple

at :: Term -> Int -> Term
at = (Fix .) . At

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


-- Pretty-printing

prettyTerm :: Int -> Term -> ShowS
prettyTerm = flip . para $ \ term d -> case term of
  Product [] -> showString "Unit"
  Product ts -> showParen (d > 7) $ foldr (.) id (intersperse (showString " * ") (map (($ 8) . snd) ts))
  Sum [] -> showString "Void"
  Sum ts -> showParen (d > 6) $ foldr (.) id (intersperse (showString " + ") (map (($ 7) . snd) ts))
  Pi n (t', t) (b', b)
    | elem n (freeVariables b'), t' == typeT -> showParen (d > 0) $ showChar '∀' . prettys n . showString " . " . b 0
    | elem n (freeVariables b') -> showParen (d > 0) $ showParen True (prettys n . showString " : " . t 1) . showString " -> " . b 0
    | otherwise -> showParen (d > 0) $ t 1 . showString " -> " . b 0
  Mu n (_, t) (_, b) -> showParen (d > 0) $ showChar 'µ' . showParen True (prettys n . showString " : " . t 1) . showString " . " . b 0
  Sigma n (_, t) (_, b) -> showBrace True $ prettys n . showString " : " . t 1 . showString " | " . b 0

  Type -> showString "Type"

  Abs n (_, b) -> showParen (d > 0) $ showChar '\\' . prettys n . showString " . " . b 0
  Var n -> prettys n
  App (_, f) (_, a) -> showParen (d > 10) $ f 10 . showChar ' ' . a 11

  Inj (_, a) i -> showParen (d > 10) $ showString "inj" . showSubscript i . showChar ' ' . a 11
  Case (_, s) cs -> showParen (d > 10) $ showString "case " . s 0 . showString " of " . showBrace True (foldr (.) id (intersperse (showString "; ") (map (($ 11) . snd) cs)))

  Tuple vs -> showParen True $ foldr (.) id (intersperse (showString ", ") (map (($ 0) . snd) vs))
  At (_, a) i -> showParen (d > 10) $ a 11 . showString " at " . showSubscript i

  Let n (_, v) (_, b) -> showParen (d > 10) $ showString "let " . prettys n . showString " = " . v 0 . showString " in " . b 0

  As (_, a) (_, t) -> showParen (d > 0) $ a 1 . showString " : " . t 0
  where showBrace b s = if b then showString "{ " . s . showString " }" else s
        showSubscript i
          | i < 0 = showChar '₋' . showSubscript (abs i)
          | i < 10 = showChar (subscripts !! i)
          | otherwise = let (n, d) = i `divMod` 10 in showSubscript n . showSubscript d
        subscripts = "₀₁₂₃₄₅₆₇₈₉"


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

  (Inj a1 i1, Inj a2 i2) | i1 == i2 -> Just (Inj (f a1 a2) i1)
  (Case s1 cs1, Case s2 cs2) | length cs1 == length cs2 -> Just (Case (f s1 s2) (zipWith f cs1 cs2))

  (Tuple vs1, Tuple vs2) | length vs1 == length vs2 -> Just (Tuple (zipWith f vs1 vs2))
  (At a1 i1, At a2 i2) | i1 == i2 -> Just (At (f a1 a2) i1)

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

    Inj a i -> Inj (f a) i
    Case s cs -> Case (f s) (map f cs)

    Tuple vs -> Tuple (map f vs)
    At a i -> At (f a) i

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

    Inj a _ -> f a
    Case s cs -> mappend (f s) (foldMap f cs)

    Tuple vs -> foldMap f vs
    At a _ -> f a

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

    Inj l i -> showParen (d > 10) $ showString "in" . showSubscript i . showChar ' ' . pp 11 l
    Case s cs -> showParen (d > 10) $ showString "case " . pp 0 s . showString " of " . foldr (.) id (intersperse (showChar ' ') (map (pp 11) cs))

    Tuple vs -> showParen True $ foldr (.) id (intersperse (showString ", ") (map (pp 0) vs))
    At a i -> showParen (d > 10) $ pp 11 a . showSubscript i

    Let n v b -> showParen (d > 10) $ showString "let " . pn 0 n . showString " = " . pp 0 v . showString " in " . pp 0 b

    As term ty -> showParen (d > 0) $ pp 1 term . showString " : " . pp 0 ty
    where showBrace b s = if b then showString "{ " . s . showString " }" else s
          showSubscript i
            | i < 0 = showChar '₋' . showSubscript (abs i)
            | i < 10 = showChar (subscripts !! i)
            | otherwise = let (n, d) = i `divMod` 10 in showSubscript n . showSubscript d
          subscripts = "₀₁₂₃₄₅₆₇₈₉"

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

    Inj a i -> showsBinaryWith spr showsPrec "Inj" d a i
    Case s cs -> showsBinaryWith spr (liftShowsPrec spr slr) "Case" d s cs

    Tuple as -> showsUnaryWith (liftShowsPrec spr slr) "Tuple" d as
    At a i -> showsBinaryWith spr showsPrec "At" d a i

    Let n v b -> showsTernaryWith spn spr spr "Let" d n v b

    As term ty -> showsBinaryWith spr spr "As" d term ty

instance Show n => Show1 (ExprF n) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $
  showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z
