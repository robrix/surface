module Surface.Judgement.Spec where

import Data.Functor.Listable
import Data.Result
import Judgement
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = do
  describe "equals" $ do
    prop "reflexivity" . forAll embedTiers $
      \ expr -> run (expr `equals` expr) `shouldBe` return ()

    prop "symmetry" . forAll (embedTiers >< embedTiers) . uncurry $
      \ a b -> eraseErrors (run (a `equals` b)) `shouldBe` eraseErrors (run (b `equals` a))

eraseErrors :: Result a -> Result a
eraseErrors = mapErrors (const [])
