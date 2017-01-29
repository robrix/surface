module Text.Pretty where

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
