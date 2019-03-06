module Task4
  ( iterateElement
  , fibonacci
  , factorial
  , mapFix
  ) where

import Data.Function(fix)

iterateElement :: a -> [a]
iterateElement a = fix (\x -> a : x)

fibonacci :: Integer -> Integer
fibonacci = fix (\fib n ->
  if n < 0 then fib (n + 2) - fib (n + 1)
    else if n == 0 then  0
      else if n == 1 then 1
        else fib (n - 2) + fib (n - 1)
  )

factorial :: Integer -> Integer
factorial = fix (\fac n ->
  if n < 0 then error "factorial for n, that < 0"
    else if n == 0 then 1
      else n * fac (n - 1)
  )

mapFix :: (a -> b) -> [a] -> [b]
mapFix f = fix (\m (a : as) ->
  if null as then [f a]
    else f a : m as
  )
