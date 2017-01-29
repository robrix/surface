module Text.Pretty where

import Data.Functor.Foldable

class Pretty t where
  prettyPrec :: t -> (Int, ShowS)

class Functor f => Pretty1 f where
  prettyPrec1 :: f (Int, ShowS) -> (Int, ShowS)

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putStrLn . pretty

prettyParen :: Int -> (Int, ShowS) -> ShowS
prettyParen d (p, s) = showParen (p >= d) s

pretty :: Pretty a => a -> String
pretty = ($ "") . snd . prettyPrec


-- Instances

instance Pretty1 f => Pretty (Fix f) where
  prettyPrec = cata prettyPrec1
