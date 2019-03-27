module TestStringSum (checkStringSum) where

import Test.Hspec
import Hw2Block1 (stringSum)

checkStringSum :: SpecWith()
checkStringSum = describe "stringSum tests" $ do
  it "simple test" $
    stringSum "1 2 3 4" `shouldBe` Just 10

  it "illegal test" $
    stringSum "Hello" `shouldBe` Nothing

  it "with negative values" $
    stringSum "1 (-2) 3 (-4)" `shouldBe` Just (-2)

  it "empty" $
    stringSum "" `shouldBe` Just 0
