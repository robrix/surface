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

  describe "lambda" $ do
    it "can take single params" $
      Parser.lambda `parseString` "\\ a . a" `shouldBe` Result (makeLambda (N "a") (Expr.var (N "a")))

    it "can take multiple params" $
      Parser.lambda `parseString` "\\ a b . a b" `shouldBe` Result (makeLambda (N "a") (makeLambda (N "b") (var (N "a") # var (N "b"))))
