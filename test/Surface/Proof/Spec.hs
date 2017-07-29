module Surface.Proof.Spec where

import Data.Functor.Listable
import Expr
import Surface.Proof hiding ((==>))
import Test.Hspec
import Test.Hspec.LeanCheck
import Test.LeanCheck

spec :: Spec
spec = do
  describe "alphaEquivalent" $ do
    prop "reflexivity" . forAll embedTiers $
      \ expr -> runProof (expr `alphaEquivalent` expr) `shouldBe` return True

    prop "symmetry" . forAll (embedTiers >< embedTiers) . uncurry $
      \ a b -> eraseErrors (runProof (a `alphaEquivalent` b)) `shouldBe` eraseErrors (runProof (b `alphaEquivalent` a))

  describe "equate" $ do
    prop "reflexivity" . forAll embedTiers $
      \ expr -> runProof (expr `equate` expr) `shouldBe` return ()

    prop "symmetry" . forAll (embedTiers >< embedTiers) . uncurry $
      \ a b -> eraseErrors (runProof (a `equate` b)) `shouldBe` eraseErrors (runProof (b `equate` a))

    prop "transitivity" . forAll (embedTiers >< embedTiers >< embedTiers) . uncurryr3 $
      \ a b c -> (succeeded (runProof (a `equate` b)) && succeeded (runProof (b `equate` c)) ==> succeeded (runProof (a `equate` c))) `shouldBe` True

    prop "congruence" . forAll (embedTiers >< embedTiers) . uncurry $
      \ a b -> runProof ((a .->. b) `equate` (a .->. b)) `shouldBe` return ()

    prop "contains beta-equivalence" . forAll (nameTiers >< embedTiers >< embedTiers) . uncurryr3 $
      \ n a b -> runProof ((makeLambda n (pair a (var n)) # b) `equate` substitute b n (pair a (var n))) `shouldBe` return ()

    prop "has functionality" . forAll ((nameTiers >< embedTiers >< embedTiers >< embedTiers) `suchThat` uncurryr4 (\ _ a1 a2 _ -> runProof (a1 `equate` a2) == return ())) . uncurryr4 $
      \ n a1 a2 b -> runProof (substitute a1 n b `equate` substitute a2 n b) `shouldBe` return ()


  describe "whnf" $ do
    prop "does not normalize inside of lambdas" . forAll (nameTiers >< embedTiers) . uncurry $
      \ n b -> runProof (whnf (makeLambda n b)) `shouldBe` return (makeLambda n b)

    prop "does not normalize inside of π-types" . forAll (nameTiers >< embedTiers >< embedTiers) . uncurryr3 $
      \ n t b -> runProof (whnf (makePi n t b)) `shouldBe` return (makePi n t b)

    prop "normalizes applications of lambdas" . forAll embedTiers $
      \ a -> runProof (whnf (lam id # a)) `shouldBe` runProof (whnf a)


eraseErrors :: Either [String] a -> Either [String] a
eraseErrors = either (Left . const []) Right

succeeded :: Either [String] a -> Bool
succeeded = either (const False) (const True)
