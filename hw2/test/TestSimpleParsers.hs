module TestSimpleParsers
  ( checkParenthesis
  , checkNumber
  ) where

import Test.Hspec
import Hw2Block3 (Parser(..), parenthesis, number)

checkParenthesis :: SpecWith()
checkParenthesis = describe "parenthesis tests" $ do
  it "empty" $
    runParser parenthesis "" `shouldBe` Just((), "")
  it "right short sequences" $ do
    runParser parenthesis "()" `shouldBe` Just((),"")
    runParser parenthesis "()()" `shouldBe` Just((),"")
    runParser parenthesis "(())" `shouldBe` Just((),"")
  it "right long sequences" $ do
    runParser parenthesis "(()()(()(())))" `shouldBe` Just((),"")
    runParser parenthesis "(()()()(()(())))" `shouldBe` Just((),"")
    runParser parenthesis "(()()(()(()))())" `shouldBe` Just((),"")
  it "wrong sequences" $ do
    runParser parenthesis "(" `shouldBe` Nothing
    runParser parenthesis "())" `shouldBe` Nothing
    runParser parenthesis "()()(()(())))" `shouldBe` Nothing

checkNumber :: SpecWith()
checkNumber = describe "number-parser tests" $ do
  it "empty" $
    runParser number "" `shouldBe` Nothing
  it "positive" $ do
    runParser number "+12345" `shouldBe` Just(12345, "")
    runParser number "34" `shouldBe` Just(34, "")
  it "negative" $
    runParser number "-123456" `shouldBe` Just(-123456, "")
  it "illegal" $ do
    runParser number "+ 4" `shouldBe` Nothing
    runParser number "(-2)" `shouldBe` Nothing
    runParser number "--3" `shouldBe` Nothing
    runParser number "++6" `shouldBe` Nothing
    runParser number "pi" `shouldBe` Nothing
