{-# LANGUAGE GADTs #-}
module REPL where

data REPL a where
  Prompt :: String -> REPL String
  Response :: String -> REPL ()
