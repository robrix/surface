{-# LANGUAGE GADTs #-}
module REPL where

import Control.Monad.Free.Freer

data REPLF a where
  Prompt :: String -> REPLF String
  Result :: String -> REPLF ()

type REPL = Freer REPLF
