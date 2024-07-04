module Main where

import Scanner
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Scanner tests" $ do
    it "correctly scans tokens" $ do
      let content = "var x = 100;"
      let expected = []
      scan content `shouldBe` expected
