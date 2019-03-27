module TestMoving (checkMoving) where

import Test.Hspec
import Hw2Block2 (moving)

checkMoving :: SpecWith()
checkMoving = describe "moving tests" $ do
  it "example test1" $
    moving 4 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]

  it "example test2" $
    moving 2 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]

  it "big test" $
    moving 5 [1..10000] `shouldBe` [1.0, 1.5, 2.0, 2.5] ++ [3.0..9998.0]  
