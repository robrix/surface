module Context where

import Expr

data Declaration = Known Type | Unknown
  deriving (Eq, Show)
