module Parser.Spec where

import Data.Result
import Expr
import Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "expr" $ do
    it "parses function types" $
      expr `parseString` "Unit -> Unit" `shouldBe` Result (unitT .->. unitT)

    it "parses product types" $
      expr `parseString` "Unit * Unit" `shouldBe` Result (unitT .*. unitT)

    it "parses sum types" $
      expr `parseString` "Unit + Unit" `shouldBe` Result (unitT .+. unitT)

  describe "lambda" $ do
    it "can take single params" $
      lambda `parseString` "\\ a . a" `shouldBe` Result (makeLambda (N "a") (Expr.var (N "a")))

    it "can take multiple params" $
      lambda `parseString` "\\ a b . a b" `shouldBe` Result (makeLambda (N "a") (makeLambda (N "b") (Expr.var (N "a") # Expr.var (N "b"))))
