import Parser.Spec as Parser
import Surface.Expr.Spec as Expr
import Surface.Judgement.Spec as Judgement
import Test.Hspec
import Test.Hspec.Core.Runner

main :: IO ()
main = hspecWith config . parallel $ do
  describe "Expr" Expr.spec
  describe "Judgement" Judgement.spec
  describe "Parser" Parser.spec
  where config = defaultConfig { configSmallCheckDepth = 1000 }
