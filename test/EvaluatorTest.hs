module EvaluatorTest (spec) where

import Evaluator
import Shared
import Test.Hspec

spec :: Spec
spec = describe "Eval tests" $ do
  it "Subtraction" $ do
    let eval = Subtract (Number 3) (Number 2)
    evaluate eval `shouldBe` 1
