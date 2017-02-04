module Text.Pretty where

import Control.Monad.Free.Freer
import Data.Functor.Foldable

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


-- Instances

instance Pretty1 f => Pretty (Fix f) where
  prettyPrec d = liftPrettyPrec prettyPrec d . unfix

instance Pretty1 f => Pretty2 (FreerF f) where
  liftPrettyPrec2 pa _ d (Pure a) = pa d a
  liftPrettyPrec2 _ pb d (Free cont r) = liftPrettyPrec (\ i -> pb i . cont) d r

instance (Pretty1 f, Pretty a) => Pretty1 (FreerF f a) where
  liftPrettyPrec = liftPrettyPrec2 prettyPrec
