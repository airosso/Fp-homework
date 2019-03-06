module Block5
  ( maybeConcat
  , eitherConcat
  , fromString
  , toString
  , getEndo
  ) where

import Data.Either (lefts, rights)
import Data.Maybe (catMaybes)
import Data.Semigroup (Semigroup (..))

maybeConcat :: Monoid a => [Maybe a] -> a
maybeConcat arr = mconcat (catMaybes arr)

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat arr = (mconcat $ lefts arr, mconcat $ rights arr)

data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty t) where
  (a :| as) <> (b :| bs) = a :| (as ++ (b : bs))

data ThisOrThat a b = This a | That b | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (This a) <> (This b)         = This (a <> b)
  (That a) <> (That b)         = That (a <> b)
  (This a) <> (That b)         = Both a b
  (That b) <> (This a)         = Both a b
  (Both a1 b1) <> (Both a2 b2) = Both (a1 <> a2) (b1 <> b2)
  (Both a b) <> (This a')      = Both (a <> a') b
  (Both a b) <> (That b')      = Both a (b <> b')
  (This a') <> (Both a b)      = Both (a <> a') b
  (That b') <> (Both a b)      = Both a (b <> b')
---доп
newtype Name = Name String

instance Semigroup Name where
  (Name s) <> (Name t) = Name (s ++ ('.' : t))

instance Monoid Name where
  mappend a (Name "") = a
  mappend (Name "") b = b
  mappend a b         = a <> b
  mempty = Name ""

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo t) where
  (Endo x) <> (Endo y) = Endo (x . y)

instance Monoid (Endo t) where
  mappend = (<>)
  mempty = Endo id

data Builder = One Char | Many [Builder]

instance Semigroup Builder where
 (One a)  <> (One b)  = Many [One a, One b]
 (One a)  <> (Many b) = Many (One a : b)
 (Many a) <> (One b)  = Many (a ++ [One b])
 (Many a) <> (Many b) = Many (a ++ b)

instance Monoid Builder where
  mappend a (Many []) = a
  mappend (Many []) a = a
  mappend a b         = a <> b
  mempty = Many []

fromString :: String -> Builder
fromString []      = Many []
fromString [a]     = One a
fromString (a: as) = Many (One a : [fromString as])

toString :: Builder -> String
toString (Many []) = ""
toString (One a)   = [a]
toString (Many a)  = foldr (mappend . toString) mempty a
