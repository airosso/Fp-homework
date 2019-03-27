module Hw2Block1 
  ( stringSum
  , Tree(..)
  , NonEmpty(..)
  ) where

import Control.Applicative (liftA2)
import Text.Read (readMaybe)

stringSum :: String -> Maybe Int
stringSum s = fmap sum (traverse readMaybe (words s))
--------------------
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch a b) = Branch (fmap f a) (fmap f b)

instance Applicative Tree where
  pure = Leaf
  Leaf a       <*> x            = fmap a x
  Branch l _   <*> Leaf a       = l <*> Leaf a
  Branch l1 r1 <*> Branch l2 r2 = Branch (l1 <*> l2) (r1 <*> r2)

instance Foldable Tree where
  foldMap f (Leaf a)     = f a
  foldMap f (Branch _ r) = foldMap f r

instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Branch l r) = liftA2 Branch (traverse f l) (traverse f r)
-------------------
data NonEmpty a = a :| [a]

instance Functor NonEmpty where
  fmap f (a :| as) = f a :| map f as

instance Applicative NonEmpty where
  pure a = a :| []
  (a :| as) <*> (b :| bs) = a b :| [x y | x <- as, y <- bs]

fromNonEmptytoList :: NonEmpty a -> [a]
fromNonEmptytoList (a :| as) = a : as

instance Monad NonEmpty where
  return a = a :| []
  (a :| as) >>= f = let (b:| bs) = f a in
                    b:| (bs ++ (as >>= fromNonEmptytoList . f))

instance Foldable NonEmpty where
  foldMap f (a :| as) = f a `mappend` foldMap f as

instance Traversable NonEmpty where
  traverse f (a :| as) = fmap (:|) (f a) <*> traverse f as
