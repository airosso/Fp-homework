module Hw2Block2
  ( Expr(..)
  , ArithmeticError(..)
  , eval
  , moving
  ) where

import Control.Monad (liftM2)
import Control.Monad.State.Lazy
import Data.Either (fromRight)

data Expr = Const Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr

data ArithmeticError = ErrorDiv
                     | ErrorPow
  deriving Eq

instance Show ArithmeticError where
  show ErrorDiv = "Division by zero"
  show ErrorPow = "Raise to a negative power"

eval :: Expr -> Either ArithmeticError Int
eval (Const a) = Right a
eval (Add a b) = liftM2 (+) (eval a) (eval b)
eval (Sub a b) = liftM2 (-) (eval a) (eval b)
eval (Mul a b) = liftM2 (*) (eval a) (eval b)
eval (Div a b) = if fromRight 0 (eval b) == 0 then Left ErrorDiv else liftM2 div (eval a) (eval b)
eval (Pow a b) = if fromRight (-1) (eval b) < 0 then Left ErrorPow
                 else liftM2 (^) (eval a) (eval b)

-------
moving :: Int -> [Int] -> [Float]
moving period list = evalState (stateFn 0) (list, list, 0)
  where
    stateFn :: Int -> State ([Int], [Int], Int) [Float]
    stateFn index = do
      (pref, after, oldSum) <- get
      if null after then return []
      else if index < period
        then do
          let newSum = oldSum + head after
              newAverage = realToFrac newSum / realToFrac (index + 1)
          put (pref, tail after, newSum)
          rest <- stateFn $ index + 1
          return $ newAverage : rest
        else do
          let (p:ps, a:as) = (pref, after)
              newSum = oldSum + a - p
              newAverage = realToFrac newSum / realToFrac period
          put (ps, as, newSum)
          rest <- stateFn $ index + 1
          return $ newAverage : rest
