{-# LANGUAGE TypeOperators #-}
module Task1
  ( distributivity
  , associator
  , eitherAssoc
  ) where

distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a)      = (Left a, Left a)
distributivity (Right (b,c)) = (Right b, Right c)

associator :: (a, (b, c)) -> ((a, b), c)
associator (a, p) = ((a, fst p), snd p)

type (<->) a b = (a -> b, b -> a)

eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (f1, f2)
  where
    f1 (Left a)          = Left (Left a)
    f1 (Right (Left a))  = Left (Right a)
    f1 (Right (Right a)) = Right a
    f2 (Left (Left a))   = Left a
    f2 (Left (Right a))  = Right (Left a)
    f2 (Right a)         = Right (Right a)
