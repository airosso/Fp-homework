{-# LANGUAGE InstanceSigs #-}
module Block4
  ( splitOn
  ) where

data Pair a = Pair a a
data NonEmpty a = a :| [a]

instance Foldable Pair where
  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair x y) = f x `mappend` f y

  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f z (Pair x y) = f x (f y z)

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x:| xs) = f x `mappend` foldMap f xs

  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f y (x:| xs) = f x (foldr f y xs)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn x arr = let (y:ys) = foldr (\c (a:as) ->
                              if c == x then [] : a : as
                              else (c : a) : as) [[]] arr
                in y :| ys
