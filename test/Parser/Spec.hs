module Parser.Spec where

import Data.Result
import Expr
import Module
import Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "expr" $ do
    it "parses function types" $
      whole expr `parseString` "Unit -> Unit" `shouldBe` Result (unitT .->. unitT)

    it "parses product types" $
      whole expr `parseString` "Unit * Unit" `shouldBe` Result (unitT .*. unitT)

    it "parses sum types" $
      whole expr `parseString` "Unit + Unit" `shouldBe` Result (unitT .+. unitT)

  describe "functionType" $ do
    it "parses right-associatively" $
      whole functionType `parseString` "a -> b -> c" `shouldBe` Parser.functionType `parseString` "a -> (b -> c)"

    it "constructs right-associated functions" $
      whole functionType `parseString` "a -> b -> c" `shouldBe` Result (varN "a" .->. (varN "b" .->. varN "c"))

    it "can take type applications" $
      whole functionType `parseString` "a b -> c" `shouldBe` Result (varN "a" # varN "b" .->. varN "c")

    it "can return type applications" $
      whole functionType `parseString` "a -> b c" `shouldBe` Result (varN "a" .->. varN "b" # varN "c")

  describe "lambda" $ do
    it "can take single params" $
      whole lambda `parseString` "\\ a . a" `shouldBe` Result (makeLambda (N "a") (varN "a"))

    it "can take multiple params" $
      whole lambda `parseString` "\\ a b . a b" `shouldBe` Result (makeLambda (N "a") (makeLambda (N "b") (varN "a" # varN "b")))

  describe "pi" $ do
    it "binds a variable" $
      whole piType `parseString` "(a : Type) -> a -> a" `shouldBe` Result (makePi (N "a") typeT (varN "a" .->. varN "a"))

    it "can occur in the body of lambdas" $
      whole lambda `parseString` "\\ p q. (c : Type) -> (p -> q -> c) -> c" `shouldBe` Result (makeLambda (N "p") (makeLambda (N "q") (makePi (N "c") typeT ((varN "p" .->. varN "q" .->. varN "c") .->. varN "c"))))

  describe "declaration" $ do
    it "parses a type and value" $
      whole declaration `parseString` "and : Type -> Type -> Type\nand = \\p q. (c : Type) -> (p -> q -> c) -> c" `shouldBe` Result (Declaration "and" (typeT .->. typeT .->. typeT) (makeLambda (N "p") (makeLambda (N "q") (makePi (N "c") typeT ((varN "p" .->. varN "q" .->. varN "c") .->. varN "c")))))
