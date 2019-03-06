module Task5
  ( Nat
  , zero
  , succChurch
  , churchPlus
  , churchMult
  , churchToInt
  ) where

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

succChurch :: Nat a -> Nat a
succChurch n f x = n f (f x)

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus n1 n2 f x = n1 f (n2 f x)

churchMult :: Nat a -> Nat a -> Nat a
churchMult n1 n2 f = n1 (n2 f)

churchToInt :: Nat Integer -> Integer
churchToInt n = n (+1) 0
