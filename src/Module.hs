{-# LANGUAGE RecordWildCards #-}
module Module where

import Data.Foldable (toList)
import Data.Functor.Foldable (unfix)
import Data.List (intersperse)
import Expr
import Text.Pretty

data Module = Module
  { moduleName :: String
  , moduleDeclarations :: [Declaration] }
  deriving (Eq, Show)

data Declaration
  = Declaration
    { declarationName :: Name
    , declarationType :: Type
    , declarationTerm :: Term }
  | Data
    { declarationName :: Name
    , declarationType :: Expr
    , declarationConstructors :: [Constructor] }
  deriving (Eq, Show)

data Constructor
  = Constructor
    { constructorName :: Name
    , constructorSignature :: Expr }
  deriving (Eq, Show)


-- Instances

instance Pretty Declaration where
  prettyPrec _ (Declaration name ty term)
    = prettyPrec 0 name . showString " : " . prettyPrec 0 ty . showChar '\n'
    . prettyPrec 0 name . showString " = " . prettyPrec 0 term . showChar '\n'
  prettyPrec _ (Data dname sig constructors)
    = showString "data " . prettyPrec 0 dname . prettyDSig sig . showString " where" . showChar '\n'
    . foldr ((.) . prettyConstructor) id constructors
    where prettyDSig :: Expr -> ShowS
          prettyDSig t = case unfix t of
            Type -> id
            _ -> showString " : " . prettyPrec 0 t
          prettyConstructor (Constructor cname sig) = showString "  " . prettyPrec 0 cname . showString " : " . prettyCSig sig
          prettyCSig :: Expr -> ShowS
          prettyCSig t = case unfix t of
            Pi n ty body -> case n of
              (I (-1)) -> prettyPrec 0 ty . showString " -> " . prettyPrec 0 body
              _ -> showParen True (prettyPrec 0 n . showString " : " . prettyPrec 0 ty) . showString " -> " . prettyPrec 0 body
            _ -> prettyPrec 0 t


instance Pretty Module where
  prettyPrec _ Module{..} = foldr (.) id (intersperse nl (mod : (prettyPrec 0 <$> toList moduleDeclarations)))
    where mod = showString "module " . showString moduleName . showString " where" . nl
          nl = showChar '\n'
