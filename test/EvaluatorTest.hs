module EvaluatorTest (spec) where

import Evaluator
import Shared
import Test.Hspec

spec :: Spec
spec = describe "Evaluator tests" $ do
  it "evaluates subtraction" $ do
    let expr = Subtract (Number 3) (Number 2)
    evaluate expr `shouldBe` IntVal 1
  it "evaluates less than" $ do
    let expr = Less (Number 3) (Number 2)
    evaluate expr `shouldBe` BoolVal False
  it "evaluates less than" $ do
    let expr = GreaterEqual (Number 3) (Number 3)
    evaluate expr `shouldBe` BoolVal True
