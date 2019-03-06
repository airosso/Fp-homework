module Task3
  ( s
  , composition
  , identity
  , contraction
  , permutation
  ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

--const:: a -> b -> a

composition :: (b -> c) -> (a -> b) -> a -> c
composition f = s (const f)

identity :: a -> a
identity a = s const (const a) a

contraction :: (a -> a -> b) -> a -> b
contraction f a = s f (const a) a

permutation :: (a -> b -> c) -> b -> a -> c
permutation f y = s f (const y)
