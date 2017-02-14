module Surface.Expr.Spec where

import Data.Functor.Listable
import Data.List (delete)
import Expr
import Test.Hspec
import Test.Hspec.LeanCheck
import Test.LeanCheck

spec :: Spec
spec = do
  describe "rename" $ do
    it "renames free variables" $
      rename (N "a") (N "b") (makeLambda (N "c") (varN "a")) `shouldBe` makeLambda (N "c") (varN "b")

    it "does not rename bound variables" $
      rename (N "a") (N "b") (makeLambda (N "a") (varN "a")) `shouldBe` makeLambda (N "a") (varN "a")

    it "renames through other syntax" $
      rename (N "a") (N "b") (makeLambda (N "c") (varN "a" # varN "c")) `shouldBe` makeLambda (N "c") (varN "b" # varN "c")

  describe "substitute" $ do
    it "replaces free variables" $
      substitute unit (N "a") (varN "b" # varN "a")  `shouldBe` varN "b" # unit

    it "renames bound variables" $
      substitute unit (N "a") (makeLambda (N "a") (varN "b" # varN "a"))  `shouldBe` makeLambda (N "b'") (varN "b" # varN "b'")

    it "picks a variable fresh in both the substitute and substitutee" $
      substitute (varI 1) (I 0) (makeLambda (I 0) (varI 0)) `shouldBe` makeLambda (I 2) (varI 2)

  describe "freeVariables" $ do
    prop "does not contain variables bound by lambdas" . forAll (nameTiers >< embedTiers) . uncurry $
      \ name body -> freeVariables (makeLambda name body) `shouldBe` delete name (freeVariables body)

    prop "does not contain variables bound by pi types" . forAll (nameTiers >< embedTiers >< embedTiers) . uncurryr3 $
      \ name ty body -> freeVariables (makePi name ty body) `shouldBe` delete name (freeVariables body)

    prop "contains free variables from type of pi types" . forAll (nameTiers >< embedTiers) . uncurry $
      \ name expr -> freeVariables (makePi name expr expr) `shouldBe` freeVariables expr

  describe "==" $ do
    prop "reflexivity" . forAll embedTiers $
      \ expr -> expr == (expr :: Expr) `shouldBe` True

    prop "symmetry" . forAll (embedTiers >< embedTiers) . uncurry $
      \ a b -> a == b `shouldBe` b == (a :: Expr)

    prop "transitivity" . forAll (embedTiers >< embedTiers >< embedTiers) . uncurryr3 $
      \ a b c -> (a == b && b == c ==> a == (c :: Expr)) `shouldBe` True
