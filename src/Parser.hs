{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, OverloadedLists #-}
module Parser where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Char
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Expr
import Module
import Text.Parser.Token
import Text.Parser.Token.Highlight hiding (Constructor)
import Text.Parser.Token.Style
import Text.Trifecta as Trifecta

newtype SurfaceParser p a = SurfaceParser { runSurfaceParser :: p a }
  deriving (Alternative, Applicative, CharParsing, Functor, Monad, Parsing)

instance TokenParsing p => TokenParsing (SurfaceParser p) where
  someSpace = SurfaceParser $ buildSomeSpaceParser someSpace haskellCommentStyle
  nesting = SurfaceParser . nesting . runSurfaceParser
  highlight h = SurfaceParser . highlight h . runSurfaceParser


parseExpr :: String -> Either [String] Expr
parseExpr = Parser.parseString (whole expr)

parseModule :: String -> Either [String] Module
parseModule = Parser.parseString (whole module')

parseString :: SurfaceParser Parser a -> String -> Either [String] a
parseString (SurfaceParser p) = toResult . Trifecta.parseString p mempty

parseFromFile :: MonadIO m => SurfaceParser Parser a -> FilePath -> m (Either [String] a)
parseFromFile (SurfaceParser p) = fmap toResult . parseFromFileEx p

whole :: (Monad m, TokenParsing m) => m a -> m a
whole p = whiteSpace *> p <* eof

toResult :: Trifecta.Result a -> Either [String] a
toResult r = case r of
  Success a -> Right a
  Failure info -> Left [show (_errDoc info)]

source :: (Monad m, TokenParsing m) => m (NonEmpty Module)
source = (:|) <$> module'
              <*> many module'
      <|> runUnlined (pure . Module "Main" <$> declaration `sepEndBy` some newline)

module' :: (Monad m, TokenParsing m) => m Module
module' = runUnlined mod
  where mod = Module <$  preword "module"
                     <*> (typeIdentifier `chainr1` ((++) <$ op ".")) <* preword "where" <* some newline
                     <*> (declaration `sepEndBy` some newline)
                     <?> "module"

declaration :: (Monad m, TokenParsing m) => m Declaration
declaration =  (datatype <?> "datatype")
           <|> (binding  <?> "declaration")
  where binding = runUnlined $ do
          name <- identifier
          Declaration (N name) <$  colon
                               <*> type' <* some newline
                               <*  token (highlight Identifier (string name)) <* op "="
                               <*> expr
        datatype = runUnlined $
          Data <$ preword "data"
               <*> (N <$> typeIdentifier)
               <*> (fromMaybe typeT <$> optional signature) <* preword "where" <* newline
               <*> some (whiteSpace *> constructor <* newline)
        constructor =
          Constructor <$> name
                      <*> signature
                      <?> "constructor"
        signature = colon *> type'

expr :: (Monad m, TokenParsing m) => m Expr
expr = type'


var :: (Monad m, TokenParsing m) => m Expr
var = Expr.var <$> name
               <?> "variable"

tuple :: (Monad m, TokenParsing m) => m Term
tuple = parens (chainr expr (pair <$ comma) Expr.unit)
               <?> "tuple"

at :: (Monad m, TokenParsing m) => m Term
at = flip Expr.at <$ preword "at" <*> int
                                  <*> expr
                                  <?> "at"

int :: (Alternative m, TokenParsing m) => m Int
int = foldl (\ into each -> into * 10 + each) 0 <$> some (digitToInt <$> digit)

lambda :: (Monad m, TokenParsing m) => m Term
lambda = foldr ((.) . makeLambda) id <$  op "\\"
                                     <*> some name <* dot
                                     <*> expr
                                     <?> "lambda"

application :: (Monad m, TokenParsing m) => m Expr
application = atom `chainl1` pure (#) <?> "function application"

inj :: (Monad m, TokenParsing m) => m Term
inj = flip Expr.inj <$ preword "inj" <*> int
                                     <*> expr
                                     <?> "inj"

case' :: (Monad m, TokenParsing m) => m Term
case' = makeCase <$  preword "case"
                 <*> expr <* preword "of"
                 <*> many (parens lambda)
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
  ty <- optional (colon *> type')
  return (maybe app (app `as`) ty)
  <?> "type annotation"


type' :: (Monad m, TokenParsing m) => m Type
type' = piType <?> "type"

atom :: (Monad m, TokenParsing m) => m Type
atom
   =  typeType
  <|> unitType
  <|> Parser.var
  <|> Parser.tuple
  <|> Parser.at
  <|> Parser.inj
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
sumType = productType `chainr1` ((.+.) <$ op "+")
                      <?> "sum type"

productType :: (Monad m, TokenParsing m) => m Type
productType = application `chainr1` ((.*.) <$ op "*")
                      <?> "product type"

piType :: (Monad m, TokenParsing m) => m Type
piType = fmap toPi $ ((:[]) <$> argument) `chainr1` ((++) <$ op "->")
  where exponential arg = case arg of
          Named name ty -> makePi name ty
          Unnamed ty -> (.->.) ty
        codomain res = case res of
          Named name ty -> Expr.var name `as` ty
          Unnamed ty -> ty
        toPi components = foldr exponential (codomain (Prelude.last components)) (Prelude.init components)

argument :: (Monad m, TokenParsing m) => m Argument
argument =  try (parens (Named <$> name <* colon <*> type'))
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
identifier =  ident (IdentifierStyle "identifier" (letter <|> char '_') (alphaNum <|> char '_') reservedWords Identifier ReservedIdentifier)
          <|> try ((:[]) <$> token (parens (highlight Operator (oneOf ".,"))))

typeIdentifier :: (Monad m, TokenParsing m) => m String
typeIdentifier = ident (IdentifierStyle "type or module identifier" (upper <|> char '_') (alphaNum <|> char '_') reservedWords Identifier ReservedIdentifier)

reservedWords :: HashSet.HashSet String
reservedWords =  [ "module", "where", "inL", "inR", "fst", "snd", "case", "of", "let", "in", "data" ]

preword :: TokenParsing m => String -> m String
preword s = token (highlight ReservedIdentifier (string s <* notFollowedBy alphaNum))
