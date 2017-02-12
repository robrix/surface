module Surface.Expr.Spec where

import Expr
import Test.Hspec

spec :: Spec
spec = do
  describe "rename" $ do
    it "renames free variables" $
      rename (N "a") (N "b") (makeLambda (N "c") (varN "a")) `shouldBe` makeLambda (N "c") (varN "b")
