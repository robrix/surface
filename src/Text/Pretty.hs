module Text.Pretty where

class Pretty t where
  prettyPrec :: t -> (Int, ShowS)
