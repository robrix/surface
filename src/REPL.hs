{-# LANGUAGE GADTs #-}
module REPL where

import Control.Monad.Free.Freer

data REPLF a where
  Prompt :: String -> REPLF String
  Result :: String -> REPLF ()

type REPL = Freer REPLF

prompt :: String -> REPL String
prompt s = Prompt s `andThen` return

result :: String -> REPL ()
result s = Result s `andThen` return


andThen :: f x -> (x -> Freer f a) -> Freer f a
andThen = (Freer .) . flip Free
