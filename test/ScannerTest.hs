module Main (main) where

import Scanner
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Scanner tests" $ do
    it "correctly scans a variable declaration" $ do
      let content = "var x = 100"
      let expected =
            [ Token VAR "var" Nothing 1,
              Token IDENTIFIER "x" Nothing 1,
              Token EQUAL "=" Nothing 1,
              Token NUMBER "100" (Just "100") 1,
              Token EOF "" Nothing 1
            ]
      scan content `shouldBe` expected

    it "correctly scans a keyword and identifier" $ do
      let content = "fun test"
      let expected =
            [ Token FUN "fun" Nothing 1,
              Token IDENTIFIER "test" Nothing 1,
              Token EOF "" Nothing 1
            ]
      scan content `shouldBe` expected

    it "correctly scans a string literal" $ do
      let content = "print \"hello\""
      let expected =
            [ Token PRINT "print" Nothing 1,
              Token STRING "\"hello\"" (Just "hello") 1,
              Token EOF "" Nothing 1
            ]
      scan content `shouldBe` expected

    it "correctly scans a simple expression" $ do
      let content = "x + 1"
      let expected =
            [ Token IDENTIFIER "x" Nothing 1,
              Token PLUS "+" Nothing 1,
              Token NUMBER "1" (Just "1") 1,
              Token EOF "" Nothing 1
            ]
      scan content `shouldBe` expected

    it "handles an unknown character" $ do
      let content = "var x = @"
      let expected =
            [ Token VAR "var" Nothing 1,
              Token IDENTIFIER "x" Nothing 1,
              Token EQUAL "=" Nothing 1,
              Token EOF "" Nothing 1 -- Stops at unknown character
            ]
      scan content `shouldBe` expected
