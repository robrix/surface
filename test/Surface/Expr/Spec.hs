module Surface.Expr.Spec where

import Expr
import Test.Hspec

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
