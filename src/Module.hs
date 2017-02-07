module Module where

import Expr

data Module = Module
  { moduleName :: String
  , moduleDeclarations :: [Declaration] }

data Declaration = Declaration
  { declarationName :: String
  , declarationType :: Type
  , declarationTerm :: Term }
