{-# LANGUAGE RecordWildCards #-}
module Module where

import Data.List (intersperse)
import Expr
import Text.Pretty

data Module = Module
  { moduleName :: String
  , moduleDeclarations :: [Declaration] }
  deriving (Eq, Show)

data Declaration = Declaration
  { declarationName :: String
  , declarationType :: Type
  , declarationTerm :: Term }
  deriving (Eq, Show)


-- Instances

instance Pretty Declaration where
  prettyPrec _ Declaration{..}
    = showString declarationName . showString " : " . prettyPrec 0 declarationType . showChar '\n'
    . showString declarationName . showString " = " . prettyPrec 0 declarationTerm . showChar '\n'

instance Pretty Module where
  prettyPrec _ Module{..} = foldr (.) id (intersperse nl (mod : (prettyPrec 0 <$> moduleDeclarations)))
    where mod = showString "module " . showString moduleName . showString " where" . nl
          nl = showChar '\n'
