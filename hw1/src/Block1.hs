module Block1
  ( order3
  , smartReplicate
  , contains
  , stringSum
  ) where

import Data.List(sort)

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) =  let ans = sort [x,y,z] in (head ans, ans !! 1, ans !! 2)

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

contains :: (Eq a) => a -> [[a]] -> [[a]]
contains a = filter (elem a)

stringSum :: String -> Int
stringSum s = sum (map read (words s))
