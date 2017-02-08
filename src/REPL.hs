{-# LANGUAGE GADTs #-}
module REPL where

import Control.Monad.Free.Freer
import Data.Result
import Expr
import Text.Pretty

data REPLF a where
  Prompt :: String -> REPLF String
  Output :: Pretty a => Result a -> REPLF ()

type REPL = Freer REPLF

data Command
  = Run Expr
  | Help
  | Quit
  | Version

prompt :: String -> REPL String
prompt s = Prompt s `andThen` return

output :: Pretty a => Result a -> REPL ()
output a = Output a `andThen` return


andThen :: f x -> (x -> Freer f a) -> Freer f a
andThen = (Freer .) . flip Free


runREPL :: REPL a -> IO a
runREPL = iterFreer alg . fmap pure
  where alg :: (x -> IO a) -> REPLF x -> IO a
        alg cont repl = case repl of
          Prompt s -> do
            putStr s
            getLine >>= cont
          Output s -> prettyPrint s >>= cont
