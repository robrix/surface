module Text.Pretty where

class Pretty t where
  prettyPrec :: t -> (Int, ShowS)

class Functor f => Pretty1 f where
  prettyPrec1 :: f (Int, ShowS) -> (Int, ShowS)

pretty :: Pretty a => a -> String
pretty = ($ "") . snd . prettyPrec
