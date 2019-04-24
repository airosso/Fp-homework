module Parser
  ( file
  , splitTokens
  , Exec (..)
  , BashString (..)
  , Parser
  , Action (..)
  , Define (..)
  ) where

import Text.Megaparsec (Parsec, option, sepEndBy1, try, anySingleBut, satisfy, between, runParser)
import Text.Megaparsec.Char (eol, char, string)
import Text.Megaparsec.Char.Lexer (lexeme, space)
import Control.Applicative
import Data.Char (isSpace, isAlpha, isDigit, isAscii)
import Data.Functor
import Data.Void

type Parser = Parsec Void String

data BashString = BashChar Char
                | InlineCommand [Exec]
                | BashDollar String
                | BashSequence [BashString]
                deriving Show

data Define = Define String BashString deriving Show

data Exec = Exec [BashString] deriving Show

data Action = ActionExec [Exec]
            | ActionDefine Define
            deriving Show

dropP :: Parser a -> Parser ()
dropP = (() <$)

skipSpaces :: Parser a -> Parser a
skipSpaces = lexeme (space (dropP (satisfy (\x -> isSpace x && x /= '\n'))) empty empty)

skipNewline :: Parser a -> Parser a
skipNewline = lexeme (space (dropP $ satisfy isSpace) empty empty)

freeSymbol :: Parser BashString
freeSymbol = BashChar <$> satisfy (\x -> not (isSpace x || x == '(' || x == ')' || x == ';'))

bashString :: Parser BashString
bashString = quote1 <|> quote2 <|> dollarCommand <|> dollarSub <|> freeSymbol

bashStr :: Parser BashString
bashStr = BashSequence <$> (skipSpaces $ some bashString)

inner :: Char -> Parser a -> Parser [a]
inner x p = try $ between (char x) (char x) (many p)

quote1 :: Parser BashString
quote1 = (BashSequence . (map BashChar)) <$> inner '\'' (anySingleBut '\'')

identifier :: Parser String
identifier = try $ (:) <$> satisfy alphaUnderscore <*> many (satisfy alphaNumUnderscore)
    where
        alphaUnderscore x = isAscii x && (isAlpha x || x == '_')
        alphaNumUnderscore x = alphaUnderscore x || isDigit x

dollarCommand :: Parser BashString
dollarCommand = try $ do
    _ <- char '$'
    inlined <- between (char '(') (char ')') commands
    return $ InlineCommand inlined


dollarSub :: Parser BashString
dollarSub = try $ do
    _ <- char '$'
    name <- identifier <|> some (satisfy isDigit)
    return $ BashDollar name

quote2 :: Parser BashString
quote2 = BashSequence <$> (inner '"' $ BashChar <$> quote2' <|> dollarCommand <|> dollarSub <|> (BashChar <$> anySingleBut '"'))

quote2' :: Parser Char
quote2' = try (string "\\\\" $> '\\' <|> string "\\\"" $> '"' <|> string "\\$" $> '$')

def :: Parser Define
def = try . skipNewline $ do
    name <- identifier
    _ <- char '='
    value <- bashStr
    return $ Define name value

command :: Parser Exec
command = try . skipSpaces $ Exec <$> (some bashStr)

commands :: Parser [Exec]
commands = (sepEndBy1 command (skipNewline $ char ';')) <* (option "" (skipNewline eol))

file :: Parser [Action]
file = many (satisfy isSpace) *> (many $ skipNewline (ActionDefine <$> def <|> ActionExec <$> commands))

splitTokens :: String -> IO [String]
splitTokens line = case runParser tokenParser "args" line of
                       Left e -> error (show e)
                       Right x -> return x
    where
        tokenParser = skipSpaces $ sepEndBy1 (try token1 <|> try token2 <|> try other) (many (satisfy isSpace))
        token1 = char '\'' *> many (anySingleBut '\'') <* (char '\'')
        token2 = char '"' *> (many (quote2' <|> anySingleBut '"')) <* (char '"')
        other = some (satisfy (not . isSpace))