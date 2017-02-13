import Test.Hspec
import Parser.Spec as Parser
import Surface.Expr.Spec as Expr
import Surface.Judgement.Spec as Judgement

main :: IO ()
main = hspec . parallel $ do
  describe "Expr" Expr.spec
  describe "Judgement" Judgement.spec
  describe "Parser" Parser.spec
