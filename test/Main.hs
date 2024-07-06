import qualified EvaluatorTest
import qualified ParserTest
import qualified ScannerTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Scanner Tests" ScannerTest.spec
  describe "Parser Tests" ParserTest.spec
  describe "Eval Tests" EvaluatorTest.spec
