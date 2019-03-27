{-# LANGUAGE LambdaCase   #-}

module Hw2Block3
  ( Parser(..)
  , ok
  , eof
  , element
  , satisfy
  , stream
  , parenthesis
  , number
  , arrays
  ) where

import Data.Bifunctor (first)
import Data.Functor
import Data.Char (isDigit, isSpace, digitToInt)
import Control.Monad ((>=>))
import Control.Applicative

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative (Parser s) where
  pure a = Parser (\x -> Just(a, x))
  (Parser a) <*> (Parser b) = Parser (a >=> (\ (x, y) -> b y >>= \ (z, t) -> return (x z, t)))
    -- $ \s -> do
    --(f, s1) <- a s
    --(b1, s2) <- b s1
    --return (f b1, s2)

instance Monad (Parser s) where
  return a = Parser (\x -> Just (a, x))
  (Parser a) >>= f = Parser (a >=> (\ (x, y) -> runParser (f x) y))

instance Alternative (Parser s) where
  empty = Parser (const Nothing)
  (Parser a) <|> (Parser b) = Parser (\x -> a x <|> b x)
---------------
ok :: Parser a ()
ok = Parser (\x -> Just((), x))

eof :: Parser a ()
eof = Parser (\case
  [] -> Just((),[])
  _ -> Nothing)

satisfy :: (a -> Bool) -> Parser a a
satisfy f = Parser (\case
  [] -> Nothing
  (a:as) -> if f a then Just (a, as) else Nothing)

element :: Eq a => a -> Parser a a
element a = satisfy (== a)

stream :: Eq a => [a] -> Parser a [a]
stream [] = ok $> []
stream (x:xs) = (:) <$> element x <*> stream xs

------
parenthesis :: Parser Char ()
parenthesis = parenthesis' 0
  where
    parenthesis' (-1) = empty
    parenthesis' x = let pp = 1 <$ element '(' <|> element ')' $> (-1)
                         end = if x == 0 then eof else empty
                     in (pp >>= parenthesis' . (+ x)) <|> end

number :: Parser Char Int
number = do
  del <- (1 <$ element '+' <|> element '-' $> (-1)) <|> ok $> 1
  absValue <- (foldl (\x y -> x * 10 + y) 0 . map digitToInt) <$> some (satisfy isDigit)
  return $ del * absValue

numbers :: Parser Char [Int]
numbers = let skipSpace = many (satisfy isSpace)
              spaceX = skipSpace *> number <* skipSpace
          in (:) <$> spaceX <*> (element ',' *> numbers) <|> ((: []) <$> spaceX)

-----------------------
arrays :: Parser Char [[Int]]
arrays = numbers >>= \a -> eof *> maybe empty (ok $>) (numbersToArrays a)
  where
    numbersToArrays [] = Just []
    numbersToArrays (x:xs) =
      if x < 0 || length xs < x
        then Nothing
        else let (cur, rest) = splitAt x xs
             in numbersToArrays rest >>= (\a -> Just $ cur : a)
