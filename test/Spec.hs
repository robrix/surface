import Test.Hspec
import Parser.Spec as Parser

main :: IO ()
main = hspec . parallel $ do
  describe "Parser" Parser.spec
