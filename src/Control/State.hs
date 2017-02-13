{-# LANGUAGE GADTs #-}
module Control.State where

import Data.Functor.Classes
import Text.Pretty

data State s a where
  Get :: State s s
  Put :: s -> State s ()


-- Instances

instance Show s => Show1 (State s) where
  liftShowsPrec _ _ d state = case state of
    Get -> showString "Get"
    Put s -> showsUnaryWith showsPrec "Put" d s

instance (Show s, Show a) => Show (State s a) where
  showsPrec = showsPrec1

instance Pretty2 State where
  liftPrettyPrec2 pp _ d state = case state of
    Get -> showString "get"
    Put s -> showParen (d > 10) $ showsUnaryWith pp "put" 10 s

instance Eq2 State where
  liftEq2 eq _ a b = case (a, b) of
    (Get, Get) -> True
    (Put a, Put b) -> eq a b
    _ -> False
