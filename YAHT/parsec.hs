module Main where

import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Graph.Inductive.Internal.FiniteMap

int :: CharParser st Int
int = do
    i1 <- digit
    ir <- many digit
    return (read (i1:ir))

-- <|> equals `mplus`
intList :: CharParser st [Int]
intList = do
    char '['
    intList' `mplus` (char ']' >> return [])
        where intList' = do
                i <- int
                r <- (char ',' >> intList') <|>
                        (char ']' >> return [])
                return (i:r)

-- Calculator language
data Expr = Value Int
        | Expr :+: Expr
        | Expr :*: Expr
        deriving (Eq, Ord, Show)

-- choice [..] equals  ..<|>..<|>..
parseExpr :: Parser Expr
parseExpr = choice
    [
        do i <- int ; return (Value i),
        between (char '(') (char ')') $ do
            e1 <- parseExpr
            op <- oneOf "+*"
            e2 <- parseExpr
            case op of
                '+' -> return (e1 :+: e2)
                '*' -> return (e1 :*: e2)
    ]

-- compute value while parsing
parseValue :: Parser Int
parseValue = choice
    [   int
    ,   between (char '(') (char ')') $ do
            e1 <- parseValue
            op <- oneOf "+*"
            e2 <- parseValue
            case op of
                '+' -> return (e1 + e2)
                '*' -> return (e1 * e2)
    ]

-- add let statements
parseValueLet :: CharParser (FiniteMap Char Int) Int
parseValueLet = choice
    [   int
    ,   do
            string "let "
            c <- letter
            char '='
            e <- parseValueLet
            string " in "
            updateState (\fm -> addToFM fm c e)
            parseValueLet
    ,   do
            c <- letter
            fm <- getState
            case lookupFM fm c of
                Nothing ->  unexpected ("variable " ++ show c ++ " unbound")
                Just i -> return i
    ,   between (char '(') (char ')') $ do
            e1 <- parseValueLet
            op <- oneOf "+*"
            e2 <- parseValueLet
            case op of
                '+' -> return (e1 + e2)
                '*' -> return (e1 * e2)
    ]

-- shortcut
parse1 p s =
    parse p "stdin" s
parse2 p init s =
    runParser p init "stdin" s

main = return $ parse2
     -- (char 'a') "a"
     -- (char 'a') "ab"
     -- (char 'a') "b"
     -- (char 'H' >> char 'a' >> char 'l') "Hal"
     -- (char 'H' >> char 'a' >> char 'l') "Hap"
     -- int "5436"
     -- intList "[1,2,3,4]"
     -- parseExpr "(3*(4+3))"
     -- parseValue "(3*(4+3))"
     -- parseValueLet emptyFM "let c=5 in ((5+4)*c)"
     -- parseValueLet emptyFM "let c=5 in ((5+4)*let x=2 in (c+x))"
     parseValueLet emptyFM "((let x=2 in 3+4)*x)"





