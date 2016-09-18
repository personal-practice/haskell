module Main where

import Control.Monad
import Data.Char (isDigit)

newtype Parser a = Parser
    { runParser :: String -> Either String (String, a) }

instance Monad Parser where
    return a = Parser (\xl -> Right (xl, a))
    fail s = Parser (\xl -> Left s)
    Parser m >>= k = Parser $ \xl ->
        case m xl of
            Left s -> Left s
            Right (xl', a) ->
                let Parser n = k a
                in n xl'

instance MonadPlus Parser where
    mzero = Parser (\xl -> Left "mzero")
    Parser p `mplus` Parser q = Parser $ \xl ->
        case p xl of
            Right a -> Right a
            Left err -> case q xl of
                Right a -> Right a
                Left _ -> Left err

char :: Char -> Parser Char
char c =
    Parser char' where
        char' [] =
            Left ("expecting " ++ show c ++ " got EOF")
        char' (x:xs)
            | x == c = Right (xs, c)
            | otherwise =
                Left ("expecting " ++ show c ++ " got " ++ show x)

-- Simple "Hello" parser
helloParser :: Parser String
helloParser = do
    char 'H' ; char 'e' ; char 'l'  ; char 'l' ; char 'o'
    return "Hello"

matchChar :: (Char -> Bool) -> Parser Char
matchChar c = Parser matchChar' where
    matchChar' [] =
        Left ("expecting char, got EOF")
    matchChar' (x:xs)
        | c x = Right (xs, x)
        | otherwise =
            Left ("expecting char, got " ++ show x)

-- Case-sensitive "hello" parser
ciHelloParser = do
    c1 <- matchChar (`elem` "Hh")
    c2 <- matchChar (`elem` "Ee")
    c3 <- matchChar (`elem` "Ll")
    c4 <- matchChar (`elem` "Ll")
    c5 <- matchChar (`elem` "Oo")
    return [c1, c2, c3, c4, c5]

anyChar :: Parser Char
anyChar =
    Parser anyChar' where
        anyChar' [] = Left ("expecting character, got EOF")
        anyChar' (x:xs) = Right (xs, x)

-- Kleene-star combinator
many :: Parser a -> Parser [a]
many (Parser p) =
    Parser many' where
        many' xl =
            case p xl of
                Left err -> Right (xl, [])
                Right (xl', a) ->
                    let Right (xl'', rest) = many' xl'
                    in Right (xl'', a:rest)

int :: Parser int
int = do
    t1 <- matchChar isDigit
    tr <- many (matchChar isDigit)
    return $ read (t1:tr)

-- [Int] using MonadPlus
intList :: Parser [Int]
intList = do
    char '['
    intList' `mplus` (char  ']' >> return [])
        where intList' = do
                i <- int
                r <- (char ',' >> intList') `mplus`
                       (char ']' >> return [])
                return (i:r)

tricky =
    (string "Hal") `mplus` (string "Hall")

eof :: Parser ()
eof =
    Parser eof'
        where
            eof' [] = Right ([], ())
            eof' xl = Left ("expecting EOF, got " ++ show (take 10 xl))

tricky2 = do
    s <- (string "Hal") `mplus` (string  "Hall")
    eof
    return s

tricky3 =
    (do s <- string "Hal"
        eof
        return s)
    `mplus`
    (do s <- string "Hall"
        eof
        return s)

main = return $ runParser
    -- helloParser  "Hello"
    -- helloParser "Hello World!"
    -- helloParser "hello World!"
    -- ciHelloParser "heLlO world!"
    -- int "54"
    -- intList "[1,2,3]"
    -- tricky "Hall"




