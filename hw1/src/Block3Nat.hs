module Block3Nat
  ( Nat
  , plus
  , mul
  , sub
  , natToInt
  , intToNat
  , isEven
  , divNat
  , modNat
  ) where

data Nat
  = Z
  | S Nat

instance Show Nat where
  show nat = show (natToInt nat)

plus :: Nat -> Nat -> Nat
plus a Z     = a
plus a (S b) = S (plus a b)

mul :: Nat -> Nat -> Nat
mul _ Z     = Z
mul a (S b) = plus a (mul a b)

sub :: Nat -> Nat -> Nat
sub Z _         = Z
sub a Z         = a
sub (S a) (S b) = sub a b

natToInt :: Nat -> Int
natToInt Z     = 0
natToInt (S k) = 1 + natToInt k

intToNat :: Int -> Nat
intToNat 0 = Z
intToNat k = S (intToNat (k-1))

instance Eq Nat where
  (==) Z Z         = True
  (==) (S _) Z     = False
  (==) Z (S _)     = False
  (==) (S a) (S b) = (==) a b

instance Ord Nat where
  compare Z (S _)     = LT
  compare (S _) Z     = GT
  compare Z Z         = EQ
  compare (S a) (S b) = compare a b

isEven :: Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S k)) = isEven k

divNat :: Nat -> Nat -> Nat
divNat _ Z   = error "division by zero"
divNat Z _   = Z
divNat n1 n2 = case compare n1 n2 of
  EQ -> S Z
  LT -> Z
  GT -> S (divNat (sub n1 n2) n2)

modNat :: Nat -> Nat -> Nat
modNat _ Z   = error "division by zero"
modNat Z _   = Z
modNat n1 n2 = case compare n1 n2 of
  EQ -> Z
  LT -> n1
  GT -> modNat (sub n1 n2) n2
