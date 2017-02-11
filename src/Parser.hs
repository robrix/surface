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
                   <*  token (highlight Identifier (string name)) <* symbolic '='
                   <*> expr
                   <?> "declaration"

expr :: (Monad m, TokenParsing m) => m Expr
expr = term <|> type'


term :: (Monad m, TokenParsing m) => m Term
term = ascription <?> "term"
  where ascription = do
          app <- application
          ty <- optional (op ":" *> type')
          return (maybe app (app `as`) ty)
          <?> "type annotation"
        application = atomicTerm `chainr1` pure (#) <?> "function application"
        atomicTerm = Parser.unit <|> tuple <|> inLP <|> inRP <|> fstP <|> sndP <|> Parser.case' <|> lambda <|> Parser.var <|> Parser.let'

        fstP = fst' <$ preword "fst" <*> term
        sndP = snd' <$ preword "snd" <*> term

        inLP = inL <$ preword "inL" <*> term
        inRP = inR <$ preword "inR" <*> term

type' :: (Monad m, TokenParsing m) => m Type
type' = exponentialType <?> "type"
  where exponentialType = multiplicativeType `chainr1` ((.->.) <$ op "->") <?> "function type"
        multiplicativeType = additiveType `chainl1` ((.*.) <$ op "*") <?> "product type"
        additiveType = atomicType `chainl1` ((.+.) <$ op "+") <?> "sum type"
        atomicType = typeTP <|> unitTP <|> parens type'
        typeTP = typeT <$ preword "Type"
        unitTP = unitT <$ preword "Unit"

unit :: (Monad m, TokenParsing m) => m Term
unit = Expr.unit <$ preword "unit"

var :: (Monad m, TokenParsing m) => m Expr
var = Expr.var <$> name

tuple :: (Monad m, TokenParsing m) => m Term
tuple = parens (term `chainr1` (pair <$ comma)) <?> "tuple"

lambda :: (Monad m, TokenParsing m) => m Term
lambda = makeLambda <$  symbol "\\"
                    <*> name <* dot
                    <*> term

case' :: (Monad m, TokenParsing m) => m Term
case' = makeCase <$  preword "case"
                 <*> term <* preword "of"
                 <*> parens lambda
                 <*> parens lambda

let' :: (Monad m, TokenParsing m) => m Term
let' = makeLet <$  preword "let"
               <*> name <*  symbolic '='
               <*> term <*  preword "in"
               <*> term


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
