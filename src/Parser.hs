{-# LANGUAGE GADTs #-}
module Parser where

import Control.Applicative
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
  where typeP = exponentialType <?> "type"
        exponentialType = multiplicativeType `chainr1` ((.->.) <$ op "->") <?> "function type"
        multiplicativeType = additiveType `chainl1` ((.*.) <$ op "*") <?> "product type"
        additiveType = atomicType `chainl1` ((.+.) <$ op "+") <?> "sum type"
        atomicType = typeTP <|> unitTP <|> parens typeP
        typeTP = typeT <$ token (string "Type")
        unitTP = unitT <$ token (string "Unit")

        termP = unitP <|> try (parens termP) <|> pairP <|> inLP <|> inRP <|> fstP <|> sndP <?> "term"
        unitP = unit <$ token (string "unit")
        pairP = parens (termP `chainr1` (pair <$ op ",")) <?> "tuple"
        inLP = inL <$ preword "inL" <*> termP
        inRP = inR <$ preword "inR" <*> termP
        fstP = fst' <$ preword "fst" <*> termP
        sndP = snd' <$ preword "snd" <*> termP

        op = token . highlight Operator . string
        preword s = token (highlight ReservedIdentifier (string s <* notFollowedBy alphaNum))
