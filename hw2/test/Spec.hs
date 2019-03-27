module Main where

import Test.Hspec
import TestStringSum
import TestEval
import TestMoving
import TestBaseParsers
import TestSimpleParsers
import TestArrays

main :: IO()
main = hspec $
  describe "Test everything" $ do
    checkStringSum
    checkEval
    checkMoving
    checkOk
    checkEof
    checkSatisfy
    checkElement
    checkStream
    checkParenthesis
    checkNumber
    checkArrays
