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
        termP = unitP <|> pairP <|> inLP <|> inRP <?> "a term"
        typeP = typeTP <|> unitTP <?> "a type"
        pairP = pair <$> (char '('
                      *> ws
                      *> termP)
                      <* ws
                     <*> (highlight Operator (char ',')
                      *> ws
                      *> termP
                      <* ws
                      <* char ')')
        inLP = inL <$> (string "inL" *> ws' *> termP)
        inRP = inR <$> (string "inR" *> ws' *> termP)
        fstP = fst' <$> (string "fst" *> ws' *> termP)
        sndP = snd' <$> (string "snd" *> ws' *> termP)

        ws = skipMany (satisfy isSpace)
        ws' = skipSome (satisfy isSpace)
