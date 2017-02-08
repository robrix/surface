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


runREPL :: REPL a -> IO a
runREPL = iterFreer alg . fmap pure
  where alg :: (x -> IO a) -> REPLF x -> IO a
        alg cont repl = case repl of
          Prompt s -> do
            putStr s
            getLine >>= cont
          Result s -> putStrLn s >>= cont
