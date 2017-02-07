module Module where

import Expr

data Module = Module
  { moduleName :: String
  , moduleBindings :: [(String, Type, Term)] }
