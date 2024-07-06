module ParserTest (spec) where

import Parser
import Shared
import Test.Hspec

spec :: Spec
spec = describe "Parser tests" $ do
  it "parses simple numbers" $ do
    let tokens = [Token NUMBER "42" (Just "42") 1, Token EOF "" Nothing 1]
    parse tokens `shouldBe` Number 42

  it "parses simple addition" $ do
    let tokens = [Token NUMBER "1" (Just "1") 1, Token PLUS "+" Nothing 1, Token NUMBER "2" (Just "2") 1, Token EOF "" Nothing 1]
    parse tokens `shouldBe` Add (Number 1) (Number 2)

  it "parses simple multiplication" $ do
    let tokens = [Token NUMBER "2" (Just "2") 1, Token STAR "*" Nothing 1, Token NUMBER "3" (Just "3") 1, Token EOF "" Nothing 1]
    parse tokens `shouldBe` Multiply (Number 2) (Number 3)

  it "parses mixed precedence operations" $ do
    let tokens = [Token NUMBER "1" (Just "1") 1, Token PLUS "+" Nothing 1, Token NUMBER "2" (Just "2") 1, Token STAR "*" Nothing 1, Token NUMBER "3" (Just "3") 1, Token EOF "" Nothing 1]
    parse tokens `shouldBe` Add (Number 1) (Multiply (Number 2) (Number 3))
