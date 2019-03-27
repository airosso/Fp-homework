module TestArrays(checkArrays) where

import Test.Hspec
import Hw2Block3 (Parser(..), arrays)

checkArrays :: SpecWith()
checkArrays = describe "arrays-parser tests" $ do
  it "empty" $
    runParser arrays "" `shouldBe` Nothing

  it "legal tests" $ do
    runParser arrays "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just([[1, 10], [5, -7, 2]], "")
    runParser arrays "1, 1, 1, 2, 2, 3, 4" `shouldBe` Just ([[1], [2], [3,4]], "")
    runParser arrays "+5, +1, -2, +56, 3, -5" `shouldBe` Just([[1, -2, 56, 3, -5]], "")

  it "illegal tests" $ do
    runParser arrays "1,+10, 5,-7, 2" `shouldBe` Nothing
    runParser arrays "-2, 1 2" `shouldBe` Nothing
