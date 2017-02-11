{-# LANGUAGE GADTs, OverloadedLists #-}
module Parser where

import Control.Applicative
import Control.Monad.IO.Class
import Data.HashSet
import Data.Result as Result
import Expr
import Module
import Text.Parser.Token
import Text.Parser.Token.Highlight
import Text.Trifecta as Trifecta

parseExpr :: String -> Result.Result Expr
parseExpr = Parser.parseString (whiteSpace *> expr <* eof)

parseModule :: String -> Result.Result Module
parseModule = Parser.parseString (whiteSpace *> module' <* eof)

parseString :: Parser a -> String -> Result.Result a
parseString p = toResult . Trifecta.parseString p mempty

parseFromFile :: MonadIO m => Parser a -> FilePath -> m (Result.Result a)
parseFromFile p = fmap toResult . parseFromFileEx p

toResult :: Trifecta.Result a -> Result.Result a
toResult r = case r of
  Success a -> Result a
  Failure info -> Error [show (_errDoc info)]

module' :: (Monad m, TokenParsing m) => m Module
module' = runUnlined mod
  where mod = Module <$  preword "module"
                     <*> typeIdentifier <* preword "where" <* some newline
                     <*> (declaration `sepEndBy` some newline)
                     <?> "module"

declaration :: (Monad m, TokenParsing m) => m Declaration
declaration = do
  name <- identifier
  Declaration name <$  colon
                   <*> expr <* some newline
                   <*  token (highlight Identifier (string name)) <* op "="
                   <*> expr
                   <?> "declaration"

expr :: (Monad m, TokenParsing m) => m Expr
expr = term <|> type'


term :: (Monad m, TokenParsing m) => m Term
term = annotation <?> "term"

termAtom :: (Monad m, TokenParsing m) => m Term
termAtom
   =  Parser.unit
  <|> tuple
  <|> Parser.fst'
  <|> Parser.snd'
  <|> Parser.inL
  <|> Parser.inR
  <|> Parser.case'
  <|> lambda
  <|> Parser.var
  <|> Parser.let'

unit :: (Monad m, TokenParsing m) => m Term
unit = Expr.unit <$  preword "unit"
                 <?> "unit"

var :: (Monad m, TokenParsing m) => m Expr
var = Expr.var <$> name
               <?> "variable"

tuple :: (Monad m, TokenParsing m) => m Term
tuple = parens (term `chainr1` (pair <$ comma))
                     <?> "tuple"

fst' :: (Monad m, TokenParsing m) => m Term
fst' = Expr.fst' <$ preword "fst" <*> term
                                  <?> "fst"

snd' :: (Monad m, TokenParsing m) => m Term
snd' = Expr.snd' <$ preword "snd" <*> term
                                  <?> "snd"

lambda :: (Monad m, TokenParsing m) => m Term
lambda = makeLambda <$  op "\\"
                    <*> name <* dot
                    <*> term
                    <?> "lambda"

application :: (Monad m, TokenParsing m) => m Term
application = termAtom `chainr1` pure (#) <?> "function application"

inL :: (Monad m, TokenParsing m) => m Term
inL = Expr.inL <$ preword "inL" <*> term
                                <?> "inL"

inR :: (Monad m, TokenParsing m) => m Term
inR = Expr.inR <$ preword "inR" <*> term
                                <?> "inR"

case' :: (Monad m, TokenParsing m) => m Term
case' = makeCase <$  preword "case"
                 <*> term <* preword "of"
                 <*> parens lambda
                 <*> parens lambda

let' :: (Monad m, TokenParsing m) => m Term
let' = makeLet <$  preword "let"
               <*> name <*  op "="
               <*> term <*  preword "in"
               <*> term
               <?> "let"

annotation :: (Monad m, TokenParsing m) => m Term
annotation = do
        app <- application
        ty <- optional (op ":" *> type')
        return (maybe app (app `as`) ty)
        <?> "type annotation"


type' :: (Monad m, TokenParsing m) => m Type
type' = functionType <?> "type"

typeAtom :: (Monad m, TokenParsing m) => m Type
typeAtom
   =  typeType
  <|> unitType
  <|> Parser.var
  <|> parens type'

unitType :: (Monad m, TokenParsing m) => m Type
unitType = unitT <$ preword "Unit"

typeType :: (Monad m, TokenParsing m) => m Type
typeType = typeT <$ preword "Type"

sumType :: (Monad m, TokenParsing m) => m Type
sumType = typeAtom `chainl1` ((.+.) <$ op "+") <?> "sum type"

productType :: (Monad m, TokenParsing m) => m Type
productType = sumType `chainl1` ((.*.) <$ op "*") <?> "product type"

functionType :: (Monad m, TokenParsing m) => m Type
functionType = productType `chainr1` ((.->.) <$ op "->") <?> "function type"


name :: (Monad m, TokenParsing m) => m Name
name = N <$> identifier

op :: TokenParsing m => String -> m String
op = token . highlight Operator . string

identifier :: (Monad m, TokenParsing m) => m String
identifier = ident (IdentifierStyle "identifier" (lower <|> char '_') (alphaNum <|> char '_') reservedWords Identifier ReservedIdentifier)

typeIdentifier :: (Monad m, TokenParsing m) => m String
typeIdentifier = ident (IdentifierStyle "type or module identifier" (upper <|> char '_') (alphaNum <|> char '_') reservedWords Identifier ReservedIdentifier)

reservedWords :: HashSet String
reservedWords =  [ "module", "where", "inL", "inR", "fst", "snd", "case", "of", "let", "in" ]

preword :: TokenParsing m => String -> m String
preword s = token (highlight ReservedIdentifier (string s <* notFollowedBy alphaNum))
