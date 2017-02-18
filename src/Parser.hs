{-# LANGUAGE GADTs, OverloadedLists #-}
module Parser where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty as NonEmpty
import Data.Result as Result
import Expr
import Module
import Text.Parser.Token
import Text.Parser.Token.Highlight
import Text.Trifecta as Trifecta

parseExpr :: String -> Result.Result Expr
parseExpr = Parser.parseString (whole expr)

parseModule :: String -> Result.Result Module
parseModule = Parser.parseString (whole module')

parseString :: Parser a -> String -> Result.Result a
parseString p = toResult . Trifecta.parseString p mempty

parseFromFile :: MonadIO m => Parser a -> FilePath -> m (Result.Result a)
parseFromFile p = fmap toResult . parseFromFileEx p

whole :: (Monad m, TokenParsing m) => m a -> m a
whole p = whiteSpace *> p <* eof

toResult :: Trifecta.Result a -> Result.Result a
toResult r = case r of
  Success a -> Result a
  Failure info -> Error [show (_errDoc info)]

source :: (Monad m, TokenParsing m) => m (NonEmpty Module)
source = (:|) <$> module'
              <*> many module'
      <|> runUnlined (pure . makeModule "Main" <$> declaration `sepEndBy` some newline)

module' :: (Monad m, TokenParsing m) => m Module
module' = runUnlined mod
  where mod = makeModule <$  preword "module"
                         <*> (typeIdentifier `chainr1` ((++) <$ op ".")) <* preword "where" <* some newline
                         <*> (declaration `sepEndBy` some newline)
                         <?> "module"

declaration :: (Monad m, TokenParsing m) => m Declaration
declaration = runUnlined $ do
  name <- identifier
  Declaration name <$  colon
                   <*> type' <* some newline
                   <*  token (highlight Identifier (string name)) <* op "="
                   <*> expr
                   <?> "declaration"

expr :: (Monad m, TokenParsing m) => m Expr
expr = type'


var :: (Monad m, TokenParsing m) => m Expr
var = Expr.var <$> name
               <?> "variable"

tuple :: (Monad m, TokenParsing m) => m Term
tuple = parens (chainr expr (pair <$ comma) Expr.unit)
               <?> "tuple"

fst' :: (Monad m, TokenParsing m) => m Term
fst' = Expr.fst' <$ preword "fst" <*> expr
                                  <?> "fst"

snd' :: (Monad m, TokenParsing m) => m Term
snd' = Expr.snd' <$ preword "snd" <*> expr
                                  <?> "snd"

lambda :: (Monad m, TokenParsing m) => m Term
lambda = foldr ((.) . makeLambda) id <$  op "\\"
                                     <*> some name <* dot
                                     <*> expr
                                     <?> "lambda"

application :: (Monad m, TokenParsing m) => m Expr
application = atom `chainl1` pure (#) <?> "function application"

inL :: (Monad m, TokenParsing m) => m Term
inL = Expr.inL <$ preword "inL" <*> expr
                                <?> "inL"

inR :: (Monad m, TokenParsing m) => m Term
inR = Expr.inR <$ preword "inR" <*> expr
                                <?> "inR"

case' :: (Monad m, TokenParsing m) => m Term
case' = makeCase <$  preword "case"
                 <*> expr <* preword "of"
                 <*> parens lambda
                 <*> parens lambda
                 <?> "case analysis"

let' :: (Monad m, TokenParsing m) => m Term
let' = makeLet <$  preword "let"
               <*> name <* op "="
               <*> expr <* preword "in"
               <*> expr
               <?> "let"

annotation :: (Monad m, TokenParsing m) => m Term
annotation = do
        app <- application
        ty <- optional (op ":" *> type')
        return (maybe app (app `as`) ty)
        <?> "type annotation"


type' :: (Monad m, TokenParsing m) => m Type
type' = piType <?> "type"

atom :: (Monad m, TokenParsing m) => m Type
atom
   =  typeType
  <|> unitType
  <|> Parser.var
  <|> tuple
  <|> Parser.fst'
  <|> Parser.snd'
  <|> Parser.inL
  <|> Parser.inR
  <|> Parser.case'
  <|> lambda
  <|> Parser.var
  <|> Parser.let'
  <|> Parser.sigma

unitType :: (Monad m, TokenParsing m) => m Type
unitType = unitT <$  preword "Unit"
                 <?> "unit type"

typeType :: (Monad m, TokenParsing m) => m Type
typeType = typeT <$  preword "Type"
                 <?> "Type"

sumType :: (Monad m, TokenParsing m) => m Type
sumType = productType `chainl1` ((.+.) <$ op "+")
                      <?> "sum type"

productType :: (Monad m, TokenParsing m) => m Type
productType = application `chainl1` ((.*.) <$ op "*")
                      <?> "product type"

piType :: (Monad m, TokenParsing m) => m Type
piType = ((:[]) <$> argument) `chainr1` ((++) <$ op "->") >>= \ components ->
  pure $! foldr exponential (codomain (Prelude.last components)) (Prelude.init components)
  where exponential arg = case arg of
          Named name ty -> makePi name ty
          Unnamed ty -> (.->.) ty
        codomain res = case res of
          Named name ty -> Expr.var name `as` ty
          Unnamed ty -> ty

argument :: (Monad m, TokenParsing m) => m Argument
argument =  try (parens (Named <$> name <* op ":" <*> type'))
        <|>            Unnamed <$> sumType
        <?> "argument"

data Argument = Named Name Type | Unnamed Type

sigma :: (Monad m, TokenParsing m) => m Type
sigma = braces $ makeSigma <$> name <* colon
                           <*> type' <* op "|"
                           <*> expr


name :: (Monad m, TokenParsing m) => m Name
name = identifier >>= \ ident -> return $ case ident of
  "_" -> I (-1)
  _ -> N ident

op :: TokenParsing m => String -> m String
op = token . highlight Operator . string

identifier :: (Monad m, TokenParsing m) => m String
identifier = ident (IdentifierStyle "identifier" (lower <|> char '_') (alphaNum <|> char '_') reservedWords Identifier ReservedIdentifier)

typeIdentifier :: (Monad m, TokenParsing m) => m String
typeIdentifier = ident (IdentifierStyle "type or module identifier" (upper <|> char '_') (alphaNum <|> char '_') reservedWords Identifier ReservedIdentifier)

reservedWords :: HashSet.HashSet String
reservedWords =  [ "module", "where", "inL", "inR", "fst", "snd", "case", "of", "let", "in" ]

preword :: TokenParsing m => String -> m String
preword s = token (highlight ReservedIdentifier (string s <* notFollowedBy alphaNum))
