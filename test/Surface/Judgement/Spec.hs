module Surface.Judgement.Spec where

import Data.Functor.Listable
import Judgement
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = do
  describe "equals" $ do
    prop "reflexivity" . forAll embedTiers $
      \ expr -> run (expr `equals` expr) `shouldBe` return ()
