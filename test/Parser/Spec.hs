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
    it "parses right-associatively" $
      Parser.functionType `parseString` "a -> b -> c" `shouldBe` Parser.functionType `parseString` "a -> (b -> c)"

    it "constructs right-associated functions" $
      Parser.functionType `parseString` "a -> b -> c" `shouldBe` Result (varN "a" .->. (varN "b" .->. varN "c"))

    it "can take type applications" $
      Parser.functionType `parseString` "a b -> c" `shouldBe` Result (varN "a" # varN "b" .->. varN "c")

    it "can return type applications" $
      Parser.functionType `parseString` "a -> b c" `shouldBe` Result (varN "a" .->. varN "b" # varN "c")

  describe "lambda" $ do
    it "can take single params" $
      Parser.lambda `parseString` "\\ a . a" `shouldBe` Result (makeLambda (N "a") (varN "a"))

    it "can take multiple params" $
      Parser.lambda `parseString` "\\ a b . a b" `shouldBe` Result (makeLambda (N "a") (makeLambda (N "b") (varN "a" # varN "b")))

  describe "pi" $ do
    it "binds a variable" $
      Parser.piType `parseString` "(a : Type) -> a -> a" `shouldBe` Result (makePi (N "a") typeT (varN "a" .->. varN "a"))

    it "can occur in the body of lambdas" $
      Parser.lambda `parseString` "\\ p q. (c : Type) -> (p -> q -> c) -> c" `shouldBe` Result (makeLambda (N "p") (makeLambda (N "q") (makePi (N "c") typeT ((varN "p" .->. varN "q" .->. varN "c") .->. varN "c"))))
