module Parser.Spec where

import Data.Result
import Expr
import Parser (parseString)
import qualified Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "expr" $ do
    it "parses function types" $
      Parser.expr `parseString` "Unit -> Unit" `shouldBe` Result (unitT .->. unitT)

    it "parses product types" $
      Parser.expr `parseString` "Unit * Unit" `shouldBe` Result (unitT .*. unitT)

    it "parses sum types" $
      Parser.expr `parseString` "Unit + Unit" `shouldBe` Result (unitT .+. unitT)

  describe "functionType" $ do
    it "associates to the right" $
      Parser.functionType `parseString` "a -> b -> c" `shouldBe` Parser.functionType `parseString` "a -> (b -> c)"

    it "can take type applications" $
      Parser.functionType `parseString` "a b -> c" `shouldBe` Result (varN "a" # varN "b" .->. varN "c")

  describe "lambda" $ do
    it "can take single params" $
      Parser.lambda `parseString` "\\ a . a" `shouldBe` Result (makeLambda (N "a") (varN "a"))

    it "can take multiple params" $
      Parser.lambda `parseString` "\\ a b . a b" `shouldBe` Result (makeLambda (N "a") (makeLambda (N "b") (varN "a" # varN "b")))
