{-# LANGUAGE FlexibleInstances #-}
module Text.Pretty where

import Control.Monad.Free.Freer
import Data.Functor.Foldable
import Data.List (intersperse)

class Pretty t where
  prettyPrec :: Int -> t -> ShowS

class Pretty1 f where
  liftPrettyPrec :: (Int -> a -> ShowS) -> Int -> f a -> ShowS

class Pretty2 p where
  liftPrettyPrec2 :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> Int -> p a b -> ShowS

prettyPrec1 :: (Pretty a, Pretty1 f) => Int -> f a -> ShowS
prettyPrec1 = liftPrettyPrec prettyPrec

prettyPrec2 :: (Pretty a, Pretty b, Pretty2 p) => Int -> p a b -> ShowS
prettyPrec2 = liftPrettyPrec2 prettyPrec prettyPrec

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putStrLn . pretty

pretty :: Pretty a => a -> String
pretty = ($ "") . prettyPrec 0

newtype PrettyOf = PrettyOf { unPrettyOf :: Int -> ShowS }

prettyLines :: Pretty a => [a] -> PrettyOf
prettyLines [] = PrettyOf (\ _ -> showString "[]")
prettyLines [ x ] = PrettyOf (\ _ -> showString "[ " . prettyPrec 0 x . showString " ]")
prettyLines (x:xs) = PrettyOf (\ _ ->  showString "[ " . prettyPrec 0 x . foldr (\ each into -> showString "\n, " . prettyPrec 0 each . into) id xs  . showString " ]")


showBracket :: Bool -> ShowS -> ShowS
showBracket b s = if b
                  then s
                  else showString "[ " . s . showString " ]"


-- Instances

instance Pretty () where
  prettyPrec _ _ = showString "()"

instance Pretty1 f => Pretty (Fix f) where
  prettyPrec d = liftPrettyPrec prettyPrec d . unfix

instance Pretty1 f => Pretty2 (FreerF f) where
  liftPrettyPrec2 pa _ d (Pure a) = pa d a
  liftPrettyPrec2 _ pb d (Free cont r) = liftPrettyPrec (\ i -> pb i . cont) d r

instance (Pretty1 f, Pretty a) => Pretty1 (FreerF f a) where
  liftPrettyPrec = liftPrettyPrec2 prettyPrec

instance (Pretty1 f, Pretty a, Pretty b) => Pretty (FreerF f a b) where
  prettyPrec = prettyPrec2

instance Pretty1 f => Pretty1 (Freer f) where
  liftPrettyPrec pa = go where go d = liftPrettyPrec2 pa go d . runFreer

instance Pretty1 [] where
  liftPrettyPrec _ _ [] = showString "[]"
  liftPrettyPrec pp _ xs = showBracket True $ foldr (.) id (intersperse (showString ", ") (pp 0 <$> xs))

instance Pretty2 Either where
  liftPrettyPrec2 pl pr d = either (pl d) (pr d)

instance Pretty2 (,) where
  liftPrettyPrec2 pa pb _ (a, b) = showParen True $ pa 0 a . showString ", " . pb 0 b

instance (Pretty2 p, Pretty a) => Pretty1 (p a) where
  liftPrettyPrec = liftPrettyPrec2 prettyPrec

instance (Pretty1 f, Pretty a) => Pretty (f a) where
  prettyPrec = prettyPrec1

instance Pretty PrettyOf where
  prettyPrec d (PrettyOf pp) = pp d
