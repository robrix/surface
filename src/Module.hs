{-# LANGUAGE RecordWildCards #-}
module Module where

import Data.Foldable (toList)
import qualified Data.HashMap.Lazy as H
import Data.List (intersperse)
import Expr
import Text.Pretty

data Module = Module
  { moduleName :: String
  , moduleDeclarations :: H.HashMap String Declaration }
  deriving (Eq, Show)

data Declaration
  = Declaration
    { declarationName :: String
    , declarationType :: Type
    , declarationTerm :: Term }
  | Data
    { declarationName :: String
    , declarationConstructors :: [Constructor] }
  deriving (Eq, Show)

data Constructor
  = Constructor
    { constructorName :: String
    , constructorSignature :: Telescope }
  deriving (Eq, Show)

data Telescope
  = Arg Name Type Telescope
  | Rec Expr Telescope
  | End Expr
  deriving (Eq, Show)


makeModule :: String -> [Declaration] -> Module
makeModule name = Module name . foldr insert H.empty
  where insert decl = H.insert (declarationName decl) decl


-- Instances

instance Pretty Declaration where
  prettyPrec _ (Declaration name ty term)
    = showString name . showString " : " . prettyPrec 0 ty . showChar '\n'
    . showString name . showString " = " . prettyPrec 0 term . showChar '\n'
  prettyPrec _ (Data dname constructors)
    = showString "data " . showString dname . showString " where" . showChar '\n'
    . foldr ((.) . prettyConstructor) id constructors
    where prettyConstructor (Constructor cname sig) = showString "  " . showString cname . showString " : " . prettyTelescope sig
          prettyTelescope t = case t of
            Arg n ty rest -> case n of
              (I (-1)) -> prettyPrec 0 ty . showString " -> " . prettyTelescope rest
              _ -> showParen True (prettyPrec 0 n . showString " : " . prettyPrec 0 ty) . showString " -> " . prettyTelescope rest
            Rec i rest -> showString dname . showChar ' ' . prettyIndex i . showString " -> " . prettyTelescope rest
            End i -> showString dname . showChar ' ' . prettyIndex i . showChar '\n'
          prettyIndex i = if i == unit then id else prettyPrec 0 i

instance Pretty Module where
  prettyPrec _ Module{..} = foldr (.) id (intersperse nl (mod : (prettyPrec 0 <$> toList moduleDeclarations)))
    where mod = showString "module " . showString moduleName . showString " where" . nl
          nl = showChar '\n'
