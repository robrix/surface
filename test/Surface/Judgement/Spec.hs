module Surface.Judgement.Spec where

import Data.Functor.Listable
import Data.Result
import Expr
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

    prop "transitivity" . forAll (embedTiers >< embedTiers >< embedTiers) . uncurryr3 $
      \ a b c -> isResult (run (a `equals` b)) == isResult (run (b `equals` c)) `shouldBe` isResult (run (a `equals` c))

    prop "congruence" . forAll (embedTiers >< embedTiers) . uncurry $
      \ a b -> run ((a .->. b) `equals` (a .->. b)) `shouldBe` return ()

eraseErrors :: Result a -> Result a
eraseErrors = mapErrors (const [])

isResult :: Result a -> Bool
isResult r | Result _ <- r = True
           | otherwise = False
