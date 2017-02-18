module Surface.Proof.Spec where

import Data.Functor.Listable
import Data.Result
import Expr
import Surface.Proof hiding ((==>))
import Test.Hspec
import Test.Hspec.LeanCheck
import Test.LeanCheck

spec :: Spec
spec = do
  describe "alphaEquivalent" $ do
    prop "reflexivity" . forAll embedTiers $
      \ expr -> run (expr `alphaEquivalent` expr) `shouldBe` return True

    prop "symmetry" . forAll (embedTiers >< embedTiers) . uncurry $
      \ a b -> eraseErrors (run (a `alphaEquivalent` b)) `shouldBe` eraseErrors (run (b `alphaEquivalent` a))

  describe "equate" $ do
    prop "reflexivity" . forAll embedTiers $
      \ expr -> run (expr `equate` expr) `shouldBe` return ()

    prop "symmetry" . forAll (embedTiers >< embedTiers) . uncurry $
      \ a b -> eraseErrors (run (a `equate` b)) `shouldBe` eraseErrors (run (b `equate` a))

    prop "transitivity" . forAll (embedTiers >< embedTiers >< embedTiers) . uncurryr3 $
      \ a b c -> (isResult (run (a `equate` b)) && isResult (run (b `equate` c)) ==> isResult (run (a `equate` c))) `shouldBe` True

    prop "congruence" . forAll (embedTiers >< embedTiers) . uncurry $
      \ a b -> run ((a .->. b) `equate` (a .->. b)) `shouldBe` return ()

    prop "contains beta-equivalence" . forAll (nameTiers >< embedTiers >< embedTiers) . uncurryr3 $
      \ n a b -> run ((makeLambda n (pair a (var n)) # b) `equate` substitute b n (pair a (var n))) `shouldBe` return ()

    prop "has functionality" . forAll ((nameTiers >< embedTiers >< embedTiers >< embedTiers) `suchThat` uncurryr4 (\ _ a1 a2 _ -> run (a1 `equate` a2) == return ())) . uncurryr4 $
      \ n a1 a2 b -> run (substitute a1 n b `equate` substitute a2 n b) `shouldBe` return ()


  describe "whnf" $ do
    prop "does not normalize inside of lambdas" . forAll (nameTiers >< embedTiers) . uncurry $
      \ n b -> run (whnf (makeLambda n b)) `shouldBe` return (makeLambda n b)

    prop "does not normalize inside of π-types" . forAll (nameTiers >< embedTiers >< embedTiers) . uncurryr3 $
      \ n t b -> run (whnf (makePi n t b)) `shouldBe` return (makePi n t b)

    prop "normalizes applications of lambdas" . forAll embedTiers $
      \ a -> run (whnf (lam id # a)) `shouldBe` run (whnf a)


eraseErrors :: Result a -> Result a
eraseErrors = mapErrors (const [])

isResult :: Result a -> Bool
isResult r | Result _ <- r = True
           | otherwise = False
