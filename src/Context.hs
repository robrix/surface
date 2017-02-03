module Context where

import Expr

data Declaration = Known Type | Unknown
  deriving (Eq, Show)

data Entry = Name := Declaration
  deriving (Eq, Show)
