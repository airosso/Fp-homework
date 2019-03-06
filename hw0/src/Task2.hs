module Task2
  ( doubleNeg
  , excludedNeg
  , pierce
  , doubleNegElim
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

--a -> (Neg a -> Void)
--a -> (a -> Void) -> Void
doubleNeg :: a -> Neg (Neg a)
doubleNeg x y = y x

-- Neg (Either a (Neg a)) -> Void =>
-- (Either a (Neg a) -> Void) -> Void =>
-- ((Either a (a -> Void)) -> Void) -> Void
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg a = (. Right) a ((. Left) a)

pierce :: ((a -> b) -> a) -> a
pierce = undefined

doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim x = x . doubleNeg
