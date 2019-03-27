module TestEval (checkEval) where

import Test.Hspec
import Hw2Block2(eval, Expr(..), ArithmeticError(..))

checkEval :: SpecWith ()
checkEval =
  describe "eval tests" $ do
    it "const test" $
      eval (Const 14) `shouldBe` Right 14

    it "add test" $
      eval (Add (Const 26) (Const 14)) `shouldBe` Right 40

    it "sub test" $
      eval (Sub (Const 100) (Const 140)) `shouldBe` Right (-40)

    it "mul test" $
      eval (Mul (Const 15) (Const 84)) `shouldBe` Right 1260

    it "div test" $
      eval (Div (Const 400) (Const 40)) `shouldBe` Right 10

    it "divizion by zero" $
      eval (Div (Const 400) (Const 0)) `shouldBe` Left ErrorDiv

    it "pow test" $
      eval (Pow (Const 2) (Const 10)) `shouldBe` Right 1024

    it "negative power" $
      eval (Pow (Const 400) (Const (-10))) `shouldBe` Left ErrorPow

    it "simple arithmetic problem" $
      eval (Div a (Const 128)) `shouldBe` Right 1
  where
    a = Add b c
    b = Mul d (Const 4)
    c = Pow (Const 2) (Const 8)
    d = Sub e (Const 128)
    e = Add (Const 64) (Const 32)
