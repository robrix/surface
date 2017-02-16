module Parser.Spec where

import Data.Result
import Expr
import Module
import Parser
import Test.Hspec
import Text.Pretty

spec :: Spec
spec = do
  describe "expr" $ do
    it "parses function types" $
      whole expr `parseString` "Unit -> Unit" `shouldBe` result (unitT .->. unitT)

    it "parses product types" $
      whole expr `parseString` "Unit * Unit" `shouldBe` result (unitT .*. unitT)

    it "parses sum types" $
      whole expr `parseString` "Unit + Unit" `shouldBe` result (unitT .+. unitT)

  describe "piType" $ do
    it "parses right-associatively" $
      whole piType `parseString` "a -> b -> c" `shouldBe` piType `parseString` "a -> (b -> c)"

    it "constructs right-associated functions" $
      whole piType `parseString` "a -> b -> c" `shouldBe` result (varN "a" .->. (varN "b" .->. varN "c"))

    it "can take type applications" $
      whole piType `parseString` "a b -> c" `shouldBe` result (varN "a" # varN "b" .->. varN "c")

    it "can return type applications" $
      whole piType `parseString` "a -> b c" `shouldBe` result (varN "a" .->. varN "b" # varN "c")

    it "binds a variable" $
      whole piType `parseString` "(a : Type) -> a -> a" `shouldBe` result (makePi (N "a") typeT (varN "a" .->. varN "a"))

    it "can end in an annotated variable" $
      whole piType `parseString` "(a : Type) -> (b : Type)" `shouldBe` result (makePi (N "a") typeT (varN "b" `as` typeT))

    it "can occur in the body of lambdas" $
      whole lambda `parseString` "\\ p q. (c : Type) -> (p -> q -> c) -> c" `shouldBe` result (makeLambda (N "p") (makeLambda (N "q") (makePi (N "c") typeT ((varN "p" .->. varN "q" .->. varN "c") .->. varN "c"))))

    it "binds looser than sums" $
      whole piType `parseString` "a -> b + c -> d" `shouldBe` result (varN "a" .->. (varN "b" .+. varN "c") .->. varN "d")

    it "binds looser than products" $
      whole piType `parseString` "a -> b * c -> d" `shouldBe` result (varN "a" .->. (varN "b" .*. varN "c") .->. varN "d")

  describe "lambda" $ do
    it "can take single params" $
      whole lambda `parseString` "\\ a . a" `shouldBe` result (makeLambda (N "a") (varN "a"))

    it "can take multiple params" $
      whole lambda `parseString` "\\ a b . a b" `shouldBe` result (makeLambda (N "a") (makeLambda (N "b") (varN "a" # varN "b")))

  describe "declaration" $ do
    it "parses a type and value" $
      whole declaration `parseString` "and : Type -> Type -> Type\nand = \\p q. (c : Type) -> (p -> q -> c) -> c" `shouldBe` result (Declaration "and" (typeT .->. typeT .->. typeT) (makeLambda (N "p") (makeLambda (N "q") (makePi (N "c") typeT ((varN "p" .->. varN "q" .->. varN "c") .->. varN "c")))))

  where result = Parse . Result
        parseString = (Parse .) . Parser.parseString


newtype Parse a = Parse { unParse :: Result a }
  deriving Eq

instance Pretty a => Show (Parse a) where
  showsPrec d = prettyPrec d . unParse
