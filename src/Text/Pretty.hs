module Text.Pretty where

import Data.Functor.Foldable

class Pretty t where
  prettyPrec :: Int -> t -> ShowS

class Pretty1 f where
  liftPrettyPrec :: (Int -> a -> ShowS) -> Int -> f a -> ShowS

prettyPrec1 :: (Pretty a, Pretty1 f) => Int -> f a -> ShowS
prettyPrec1 = liftPrettyPrec prettyPrec

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putStrLn . pretty

pretty :: Pretty a => a -> String
pretty = ($ "") . prettyPrec 0


-- Instances

instance Pretty1 f => Pretty (Fix f) where
  prettyPrec d = liftPrettyPrec prettyPrec d . unfix
