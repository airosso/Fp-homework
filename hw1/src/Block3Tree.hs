module Block3Tree
  ( Tree
  , isEmpty
  , countElems
  , findElem
  , insertElem
  , fromList
  , findMin
  , deleteElem
  ) where

import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.List (head, tail, length)

data Tree a
  = Leaf
  | Node (NonEmpty a) (Tree a) (Tree a) deriving (Show)

isEmpty :: Tree a -> Bool
isEmpty tree = case tree of
  Leaf -> True
  _    -> False

countElems :: Tree a -> Int
countElems tree = case tree of
  Node a left right -> length a + countElems left + countElems right
  _                 -> 0

findElem :: Ord a => a -> Tree a -> Tree a
findElem a (Node list left right) | a == head (toList list) = Node list left right
                                  | a < head (toList list)  = findElem a left
                                  | otherwise               = findElem a right
findElem _ Leaf = Leaf

insertElem :: Ord a => a -> Tree a -> Tree a
insertElem a (Node list left right) | a == head (toList list) = Node (a :| toList list) left right
                                    | a < head (toList list)  = Node list (insertElem a left) right
                                    | otherwise               = Node list left (insertElem a right)
insertElem a Leaf = Node (a :| []) Leaf Leaf

fromList :: Ord a => [a] -> Tree a
fromList list = f list Leaf where
  f as tree = foldl (flip insertElem) tree as

findMin :: Ord a => Tree a -> a
findMin Leaf = undefined
findMin (Node list left _) = case left of
  Leaf -> head (toList list)
  _    -> findMin left

deleteElem :: Ord a => a -> Tree a -> Tree a
deleteElem _ Leaf = Leaf
deleteElem a (Node list left right) | a < head (toList list) = Node list (deleteElem a left) right
                                    | a > head (toList list) = Node list left (deleteElem a right)
                                    | otherwise              = f (tail (toList list)) left right
  where
    f (e : es) l r = Node (e :| es) l r
    f [] Leaf r    = r
    f [] l Leaf    = l
    f [] l r       = Node (findMin r :| []) l (deleteElem (findMin r) r)
