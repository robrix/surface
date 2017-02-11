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
