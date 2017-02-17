import Parser.Spec as Parser
import Surface.Expr.Spec as Expr
import Surface.Proof.Spec as Proof
import Test.Hspec
import Test.Hspec.Core.Runner

main :: IO ()
main = hspecWith config . parallel $ do
  describe "Expr" Expr.spec
  describe "Proof" Proof.spec
  describe "Parser" Parser.spec
  where config = defaultConfig { configSmallCheckDepth = 1000 }
