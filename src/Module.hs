{-# LANGUAGE RecordWildCards #-}
module Module where

import Data.Foldable (toList)
import Data.Functor.Foldable (unfix)
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
    , declarationType :: Expr
    , declarationConstructors :: [Constructor] }
  deriving (Eq, Show)

data Constructor
  = Constructor
    { constructorName :: String
    , constructorSignature :: Expr }
  deriving (Eq, Show)


makeModule :: String -> [Declaration] -> Module
makeModule name = Module name . foldr insert H.empty
  where insert decl = H.insert (declarationName decl) decl


-- Instances

instance Pretty Declaration where
  prettyPrec _ (Declaration name ty term)
    = showString name . showString " : " . prettyPrec 0 ty . showChar '\n'
    . showString name . showString " = " . prettyPrec 0 term . showChar '\n'
  prettyPrec _ (Data dname sig constructors)
    = showString "data " . showString dname . prettyDSig sig . showString " where" . showChar '\n'
    . foldr ((.) . prettyConstructor) id constructors
    where prettyDSig :: Expr -> ShowS
          prettyDSig t = case unfix t of
            TypeT -> id
            _ -> showString " : " . prettyPrec 0 t
          prettyConstructor (Constructor cname sig) = showString "  " . showString cname . showString " : " . prettyCSig sig
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
