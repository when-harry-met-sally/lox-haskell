module ParserTest (spec) where

import Parser
import Shared
import Test.Hspec

spec :: Spec
spec =
  describe "Parser tests" $ do
    it "It parses good" $ do
      let content = "var x = 100"
      let expected = "var x = 100"
      content `shouldBe` expected
