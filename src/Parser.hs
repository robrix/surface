{-# LANGUAGE GADTs #-}
module Parser where

import Control.Applicative
import Data.Char
import Data.Result as Result
import Expr
import Text.Parser.Token.Highlight
import Text.Trifecta

parse :: String -> Result.Result Expr
parse s = case parseString parser mempty s of
  Success a -> Result a
  Failure info -> Error [show (_errDoc info)]

parser :: Parser Expr
parser = termP <|> typeP
  where typeTP = typeT <$ string "Type"
        unitP = unit <$ string "unit"
        unitTP = unitT <$ string "Unit"
        termP = unitP <|> pairP <?> "a term"
        typeP = typeTP <|> unitTP <?> "a type"
        pairP = pair <$> (char '('
                      *> skipMany (satisfy isSpace)
                      *> termP)
                      <* skipMany (satisfy isSpace)
                     <*> (highlight Operator (char ',')
                      *> skipMany (satisfy isSpace)
                      *> termP
                      <* skipMany (satisfy isSpace)
                      <* char ')')
