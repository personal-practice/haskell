{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PostfixOperators #-}
module ParserUtils where

import Control.Applicative        ((<|>))
import Control.Monad              (void)
import Control.Monad.State
import Control.Monad.Trans.Except

import Parser

-- | Produce error on the outer level of the monad stack.
errS :: String -> ParserT a
errS = StateT . const . except . Left . ErrorMsg

-- | Produce error on the inner level of the monad stack.
errE :: String -> Except ErrorMsg (a, String)
errE = except . Left . ErrorMsg

anyChar :: ParserT Char
anyChar = StateT $ \case
  (c:cs) -> return (c, cs)
  []     -> errE "Expecting character"

-- | Parse string in full.
eof :: ParserT ()
eof = StateT $ \case
  [] -> return ((), "")
  _  -> errE "Expecting EOF"

-- | Discard the result of a single application of the given parser.
skip :: ParserT a -> ParserT ()
skip = void

-- | Discard results of many consecutive applications of the given parser.
skipMany :: ParserT a -> ParserT ()
skipMany = void . many . skip

-- | Apply given parsing multiple times, gathering them in a list.
many, (^*) :: ParserT a -> ParserT [a]
(^*) = many
many p = (p >>= \x -> (:) <$> pure x <*> many p)
    <|> return []

-- | Optional application of the given parser, returns [] on failure.
optional, (??) :: ParserT a -> ParserT [a]
(??) = optional
optional p = (p >>= \x -> return [x]) <|> return []

-- | Succeeds when next character is one of the given characters.
oneOf :: String -> ParserT Char
oneOf []     = errS "oneOf: No characters given"
oneOf [c]    = char c
oneOf (c:cs) = oneOf [c] <|> oneOf cs

-- | Parses the given char.
char :: Char -> ParserT Char
char c = StateT $ \case
  (c':cs') ->
    if c' == c then
      return (c', cs')
    else
      errE $ "Expected " ++ show c ++ " but got " ++ show c'
  []       -> errE $ "Expecting " ++ show c

-- | Parses the given string.
string :: String -> ParserT ()
string = foldr ((>>). char) (return ())

-- | Parse a whitespace character.
space :: ParserT ()
space = void $ oneOf [' ', '\t', '\n']

-- | Parse multiple whitespace characters.
spaces :: ParserT ()
spaces = void (space^*)

-- | Surround given parser with whitespace.
lexeme :: ParserT a -> ParserT a
lexeme p = spaces *> p <* spaces

infixl 6 <~
(<~) :: ParserT a -> String -> ParserT a
(<~) p s = p <* lexeme (string s)

infixr 7 ~>
(~>) :: String -> ParserT a -> ParserT a
(~>) s p = lexeme (string s) *> p

-- | Enclose a parser with given begin and ending strings.
enclose :: String -> String -> ParserT a -> ParserT a
enclose begin end p = begin ~> p <~ end

-- | Enclose a parser with parentheses.
parens :: ParserT a -> ParserT a
parens = enclose "(" ")"

-- | Enclose a parser with curly braces.
braces :: ParserT a -> ParserT a
braces = enclose "{" "}"

-- | Enclose a parser with quotes.
quotes :: ParserT a -> ParserT a
quotes = enclose "\"" "\""

-- | Parse a list of elements.
-- (i.e. enclosed by square brackets and comma-seperated)
list :: ParserT a -> ParserT [a]
list p = enclose "[" "]" (p `sepBy` ",")

-- | Apply given multiple multiple times (separated by given string) and
-- collect all results in a list.
sepBy :: ParserT a -> String -> ParserT [a]
sepBy p sep = ((:) <$> p <*> many (sep ~> p))
          <|> (p >>= \x -> return [x])
          <|> return []

-- | Excludes several characters from a parser of characters.
without :: ParserT Char -> String -> ParserT Char
without p excluded = do
  c <- p
  if c `elem` excluded then
    errS "without: found excluded character"
  else
    return c

-- | Optionally enclose in parentheses.
optParens :: ParserT a -> ParserT a
optParens p = parens p <|> p
