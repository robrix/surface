{-# LANGUAGE DeriveFunctor #-}
module Data.Result where

data Result e a = Result a | Error [e]
  deriving (Eq, Functor, Show)
