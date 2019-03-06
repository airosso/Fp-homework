module Block2
  ( remove
  , randomIntList
  , mergeSort
  ) where

import Data.List (head, tail, length)
import System.Random (newStdGen, randomRs)

remove :: Int -> [a] -> (a, [a])
remove i arr | i >= 0    = (ans, f ++ s')
             | otherwise = error "index < 0"
    where
      (f, s) =  splitAt i arr
      (ans, s') = if not $ null s then (head s, tail s) else error "No such element"

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

mergeSort :: (Ord a, Num a) => [a] -> [a]
mergeSort = f where
  f [a] = [a]
  f arr = merge (f left) (f right)
    where
      (left, right) = splitAt (length arr `div` 2) arr
      merge :: Ord a => [a] -> [a] -> [a]
      merge [] b = b
      merge a [] = a
      merge (a : as) (b : bs) | a < b     = a : merge as (b : bs)
                              | otherwise = b : merge (a : as) bs
