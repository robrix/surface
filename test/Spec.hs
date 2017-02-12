import Test.Hspec
import Parser.Spec as Parser
import Surface.Expr.Spec as Expr

main :: IO ()
main = hspec . parallel $ do
  describe "Expr" Expr.spec
  describe "Parser" Parser.spec
