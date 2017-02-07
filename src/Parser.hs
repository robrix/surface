{-# LANGUAGE GADTs #-}
module Parser where

import Control.Applicative
import Data.Char
import Data.Result as Result
import Expr
import Text.Parser.Token
import Text.Parser.Token.Highlight
import Text.Trifecta

parse :: String -> Result.Result Expr
parse s = case parseString parser mempty s of
  Success a -> Result a
  Failure info -> Error [show (_errDoc info)]

parser :: Parser Expr
parser = whiteSpace  *> (termP <|> typeP) <* eof
  where typeP = exponentialType <?> "a type"
        exponentialType = multiplicativeType `chainr1` ((.->.) <$ op "->") <?> "function type"
        multiplicativeType = additiveType `chainl1` ((.*.) <$ op "*") <?> "product type"
        additiveType = atomicType `chainl1` ((.+.) <$ op "+") <?> "sum type"
        atomicType = typeTP <|> unitTP <|> parens typeP
        typeTP = typeT <$ token (string "Type")
        unitTP = unitT <$ token (string "Unit")

        termP = unitP <|> try (parens termP) <|> pairP <|> inLP <|> inRP <|> fstP <|> sndP <?> "a term"
        unitP = unit <$ token (string "unit")
        pairP = pair <$ token (char '(')
                    <*> termP
                     <* token (highlight Operator (char ','))
                    <*> termP
                     <* token (char ')')
        inLP = inL <$> (string "inL" *> ws' *> termP)
        inRP = inR <$> (string "inR" *> ws' *> termP)
        fstP = fst' <$> (string "fst" *> ws' *> termP)
        sndP = snd' <$> (string "snd" *> ws' *> termP)

        ws = skipMany (satisfy isSpace)
        ws' = skipSome (satisfy isSpace)

        op s = symbol s *> ws
