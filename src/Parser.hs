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
        declaration = do
          name <- identifier
          Declaration name <$  colon
                           <*> expr <* some newline
                           <*  token (highlight Identifier (string name)) <* symbolic '='
                           <*> expr
                           <?> "declaration"

expr :: (Monad m, TokenParsing m) => m Expr
expr = termP <|> typeP
  where typeP = exponentialType <?> "type"
        exponentialType = multiplicativeType `chainr1` ((.->.) <$ op "->") <?> "function type"
        multiplicativeType = additiveType `chainl1` ((.*.) <$ op "*") <?> "product type"
        additiveType = atomicType `chainl1` ((.+.) <$ op "+") <?> "sum type"
        atomicType = typeTP <|> unitTP <|> parens typeP
        typeTP = typeT <$ preword "Type"
        unitTP = unitT <$ preword "Unit"

        termP = ascription <?> "term"
        ascription = do
          app <- application
          ty <- optional (op ":" *> typeP)
          return (maybe app (app `as`) ty)
          <?> "type annotation"
        application = atomicTerm `chainr1` pure (#) <?> "function application"
        atomicTerm = unitP <|> pairP <|> inLP <|> inRP <|> fstP <|> sndP <|> caseP <|> lambdaP <|> varP <|> letP
        unitP = unit <$ preword "unit"

        pairP = parens (termP `chainr1` (pair <$ comma)) <?> "tuple"
        fstP = fst' <$ preword "fst" <*> termP
        sndP = snd' <$ preword "snd" <*> termP

        inLP = inL <$ preword "inL" <*> termP
        inRP = inR <$ preword "inR" <*> termP
        caseP = makeCase <$  preword "case"
                         <*> termP <* preword "of"
                         <*> parens lambdaP
                         <*> parens lambdaP

        lambdaP = makeLambda <$  symbol "\\"
                             <*> name <* dot
                             <*> termP

        varP = var <$> name

        letP = makeLet <$  preword "let"
                       <*> name <*  symbolic '='
                       <*> termP <*  preword "in"
                       <*> termP

        name = N <$> identifier

        op = token . highlight Operator . string

identifier :: (Monad m, TokenParsing m) => m String
identifier = ident (IdentifierStyle "identifier" (lower <|> char '_') (alphaNum <|> char '_') reservedWords Identifier ReservedIdentifier)

typeIdentifier :: (Monad m, TokenParsing m) => m String
typeIdentifier = ident (IdentifierStyle "type or module identifier" (upper <|> char '_') (alphaNum <|> char '_') reservedWords Identifier ReservedIdentifier)

reservedWords :: HashSet String
reservedWords =  [ "module", "where", "inL", "inR", "fst", "snd", "case", "of", "let", "in" ]

preword :: TokenParsing m => String -> m String
preword s = token (highlight ReservedIdentifier (string s <* notFollowedBy alphaNum))
