{-# LANGUAGE GADTs, RankNTypes #-}
module Judgement where

import Control.Monad
import qualified Control.Monad.Fail as Fail
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Result
import Data.Semigroup hiding (Sum, Product)
import Expr
import Text.Pretty

data Judgement a where
  Check :: Context -> Term -> Type -> Judgement ()
  Infer :: Context -> Term -> Judgement Type

  IsType :: Context -> Term -> Judgement ()

data Goal f a where
  Failure :: [String] -> Goal f a
  Return :: a -> Goal f a
  Then :: f x -> (x -> Goal f a) -> Goal f a

type Context = [(Name, Type)]

data Declaration = Known Type | Unknown
  deriving (Eq, Show)

data Entry = Name := Declaration
  deriving (Eq, Show)


infer :: Context -> Term -> Goal Judgement Type
infer context term = Infer context term `Then` Return

check :: Context -> Term -> Type -> Goal Judgement ()
check context term ty = Check context term ty `Then` Return

isType :: Context -> Term -> Goal Judgement ()
isType context term = IsType context term `Then` Return

fresh :: Context -> Goal Judgement Type
fresh = return . var . maybe (Name 0) getMax . sfoldMap (Max . fst)

sfoldMap :: (Semigroup s, Foldable t) => (a -> s) -> t a -> Maybe s
sfoldMap f = getOption . foldMap (Option . Just . f)


define :: Name -> Type -> Context -> Context
define name ty = ((name, ty):)

lookupName :: Name -> Context -> Goal Judgement Type
lookupName name context = case lookup name context of
    Just ty -> return ty
    _ -> fail ("No variable " ++ pretty name ++ " in context.")

decompose :: Judgement a -> Goal Judgement a
decompose judgement = case judgement of
  Infer context term -> case unfix term of
    Pair x y -> do
      a <- infer context x
      b <- infer context y
      return (a .*. b)

    Fst p -> do
      ty <- infer context p
      case unfix ty of
        Product a _ -> return a
        _ -> fail ("Expected a product type, but got " ++ pretty ty)

    Snd p -> do
      ty <- infer context p
      case unfix ty of
        Product _ b -> return b
        _ -> fail ("Expected a product type, but got " ++ pretty ty)

    InL l -> do
      a <- infer context l
      b <- fresh context
      return (a .+. b)

    InR r -> do
      a <- fresh context
      b <- infer context r
      return (a .+. b)

    Case subject ifL ifR -> do
      ty <- infer context subject
      case unfix ty of
        Sum l r -> do
          b <- fresh context
          check context (l .->. b) ifL
          check context (r .->. b) ifR
          return b
        _ -> fail ("Expected a sum type, but got " ++ pretty ty)

    Unit -> return unitT

    Var name -> lookupName name context

    Abs name body -> do
      t <- fresh context
      bodyT <- infer (define name t context) body
      return (t .->. bodyT)

    App f arg -> do
      ty <- infer context f
      case unfix ty of
        Function a b -> do
          check context arg a
          return b
        _ -> fail ("Expected a function type, but got " ++ pretty ty)

    -- Types
    UnitT -> return typeT
    TypeT -> return typeT -- Impredicativity.
    Function{} -> isType context term >> return typeT
    Product{} -> isType context term >> return typeT
    Sum{} -> isType context term >> return typeT

  Check context term ty -> do
    ty' <- infer context term
    unless (ty' == ty) $ fail ("Expected " ++ pretty ty ++ " but got " ++ pretty ty')

  IsType context ty -> case unfix ty of
    UnitT -> return ()
    TypeT -> return ()
    Sum a b -> do
      isType context a
      isType context b
    Product a b -> do
      isType context a
      isType context b
    Function a b -> do
      isType context a
      isType context b

    Var name -> do
      ty <- lookupName name context
      isType context ty

    _ -> fail ("Expected a Type but got " ++ pretty ty)


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
    Check context term ty -> showsTernaryWith showsPrec showsPrec showsPrec "Check" d context term ty
    Infer context term -> showsBinaryWith showsPrec showsPrec "Infer" d context term

    IsType context ty -> showsBinaryWith showsPrec showsPrec "IsType" d context ty

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
