{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, TypeOperators, UndecidableInstances #-}
module Text.Pretty where

import Control.Monad.Effect.Fail
import Control.Monad.Effect.Internal hiding (inj)
import Control.Monad.Effect.State
import Control.Monad.Free.Freer
import Data.Foldable (toList)
import Data.Functor.Classes
import Data.Functor.Foldable
import qualified Data.HashMap.Lazy as H
import Data.List (intersperse)
import Data.List.NonEmpty hiding (intersperse, map, toList)

class Pretty t where
  prettyPrec :: Int -> t -> ShowS

  prettyList :: [t] -> ShowS
  prettyList = showListWith (prettyPrec 0)

class Pretty1 f where
  liftPrettyPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS

  liftPrettyList :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f a] -> ShowS
  liftPrettyList pp pl = showListWith (liftPrettyPrec pp pl 0)

class Pretty2 p where
  liftPrettyPrec2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> p a b -> ShowS

  liftPrettyList2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> [p a b] -> ShowS
  liftPrettyList2 ppa pla ppb plb = showListWith (liftPrettyPrec2 ppa pla ppb plb 0)

prettyPrec1 :: (Pretty a, Pretty1 f) => Int -> f a -> ShowS
prettyPrec1 = liftPrettyPrec prettyPrec prettyList

prettyPrec2 :: (Pretty a, Pretty b, Pretty2 p) => Int -> p a b -> ShowS
prettyPrec2 = liftPrettyPrec2 prettyPrec prettyList prettyPrec prettyList

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putStrLn . pretty

pretty :: Pretty a => a -> String
pretty = ($ "") . prettys

prettys :: Pretty a => a -> ShowS
prettys = prettyPrec 0

data PrettyOf a = PrettyOf { prettyPrecOf :: Int -> a -> ShowS, unPrettyOf :: a }

prettyOf :: Pretty a => a -> PrettyOf a
prettyOf = PrettyOf prettyPrec

prettyLines :: Pretty a => [a] -> PrettyOf [a]
prettyLines = PrettyOf $ \ _ lines -> case lines of
  [] -> showString "[]"
  xs -> showString "[ " . foldr (.) id (intersperse (showString "\n, ") (map (prettyPrec 0) xs)) . showString " ]"


showBracket :: Bool -> ShowS -> ShowS
showBracket b s = if b
                  then showString "[ " . s . showString " ]"
                  else s

showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith f = iff null (const (showString "[]")) (showBracket True . foldr (.) id . intersperse (showString ", ") . fmap f)

showAsListWith :: Foldable t => (a -> ShowS) -> t a -> ShowS
showAsListWith f = showListWith f . toList

iff :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
iff test con alt a = (if test a then con else alt) a


-- Instances

instance Pretty Char where
  prettyPrec = showsPrec
  prettyList = showList

instance Pretty () where
  prettyPrec _ _ = showString "()"

instance Pretty1 f => Pretty (Fix f) where
  prettyPrec d = liftPrettyPrec prettyPrec prettyList d . unfix

instance Pretty1 f => Pretty1 (Freer f) where
  liftPrettyPrec pp pl = go
    where go d (Return a) = pp d a
          go d (Then r t) = liftPrettyPrec (\ i -> go i . t) (liftPrettyList pp pl . fmap t) d r

instance (Pretty1 f, Pretty a) => Pretty (Freer f a) where
  prettyPrec = prettyPrec1

instance Pretty1 [] where
  liftPrettyPrec _ pl _ = pl

instance Pretty a => Pretty [a] where
  prettyPrec = prettyPrec1

instance Pretty1 NonEmpty where
  liftPrettyPrec pp _ _ = showAsListWith (pp 0)

instance Pretty a => Pretty (NonEmpty a) where
  prettyPrec = prettyPrec1

instance Pretty2 Either where
  liftPrettyPrec2 pl _ pr _ d = either (pl d) (pr d)

instance Pretty l => Pretty1 (Either l) where
  liftPrettyPrec = liftPrettyPrec2 prettyPrec prettyList

instance (Pretty l, Pretty r) => Pretty (Either l r) where
  prettyPrec = prettyPrec1

instance Pretty2 (,) where
  liftPrettyPrec2 pa _ pb _ _ (a, b) = showParen True $ pa 0 a . showString ", " . pb 0 b

instance Pretty l => Pretty1 ((,) l) where
  liftPrettyPrec = liftPrettyPrec2 prettyPrec prettyList

instance (Pretty l, Pretty r) => Pretty (l, r) where
  prettyPrec = prettyPrec1

instance Pretty2 H.HashMap where
  liftPrettyPrec2 pk _ pv _ _ = showAsListWith (uncurry pair) . H.toList
    where pair k v = pk 0 k . showString " : " . pv 0 v

instance Pretty (PrettyOf a) where
  prettyPrec d (PrettyOf pp a) = pp d a

instance Eq a => Eq (PrettyOf a) where
  PrettyOf _ a1 == PrettyOf _ a2 = a1 == a2


instance (Pretty1 t, Pretty1 (Union ts)) => Pretty1 (Union (t ': ts)) where
  liftPrettyPrec pp pl d u = case decompose u of
    Right t -> liftPrettyPrec pp pl d t
    Left u' -> liftPrettyPrec pp pl d u'

instance Pretty1 (Union '[]) where
  liftPrettyPrec _ _ _ _ = id

instance Pretty1 (Union effs) => Pretty1 (Eff effs) where
  liftPrettyPrec pp pl = go
    where go d (Val a) = pp d a
          go d (E r t) = liftPrettyPrec (\ i -> go i . apply t) (liftPrettyList pp pl . fmap (apply t)) d r

instance (Pretty1 (Union effs), Pretty a) => Pretty (Eff effs a) where
  prettyPrec = prettyPrec1

instance Pretty1 Fail where
  liftPrettyPrec _ _ d (Fail s) = showParen (d > 10) $ showsUnaryWith showsPrec "Fail" d s

instance Pretty s => Pretty1 (State s) where
  liftPrettyPrec _ _ _ Get = showString "Get"
  liftPrettyPrec _ _ d (Put s) = showParen (d > 10) $ showsUnaryWith prettyPrec "Put" d s
