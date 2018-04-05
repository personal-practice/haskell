{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
import Test.Tasty
import Test.Tasty.HUnit

import Control.Applicative (empty, (<|>))
import Control.Monad       (replicateM)

import Control.Monad.State

import GHC.Generics

import GenParser hiding (Config)
import Parser
import ParserUtils

main :: IO ()
main =
  defaultMain $ testGroup "Tests"
    [ testGroup "Parser Instances"
        [ testCase "Functor" $
            let p = Parser $ \s -> Right (read s :: Int, "")
            in  parse ((+ 1) <$> p) "123" @?= Right (124, "")
        , testCase "Applicative" $
            let p = (+) <$> readInt <*> readInt
            in  parse p "12..." @?= Right (3, "...")
        , testCase "Monad" $
            let p = do
                  x <- readInt
                  readPlus
                  y <- readInt
                  return (x + y)
            in  parse p "1+2..." @?= Right (3, "...")
        , testCase "Alternative" $
            let p = empty
                p' = do
                  digits <- replicateM 3 readChar
                  return (read digits :: Int)
            in  parse (p <|> p') "123..." @?= Right (123, "...")
        ]
    , testGroup "ParserT Instances"
        [ testCase "Functor" $
            let p = StateT $ \s -> Right (read s :: Int, "")
            in  runStateT ((+ 1) <$> p) "123" @?= (Right (124, "") :: Either ErrorMsg (Int, String))
        , testCase "Applicative" $
            let p = (+) <$> readIntT <*> readIntT :: ParserT Int
            in  runParserT p "12..." @?= Right (3, "...")
        , testCase "Monad" $
            let p = do
                  x <- readIntT
                  readPlusT
                  y <- readIntT
                  return (x + y)
            in  runParserT p "1+2..." @?= Right (3, "...")
        , testCase "Alternative" $
            let p = errS "error"
                p' = do
                  digits <- replicateM 3 readCharT
                  return (read digits :: Int)
            in  runParserT (p <|> p') "123..." @?= Right (123, "...")
        ]
    , testGroup "Generic Parsing"
        [ testGroup "base types"
            [ testGroup "bool"
                [ testCase "True" $ "True" ~~ True
                , testCase "False" $ "False" ~~ False
                ]
            , testGroup "char"
                [ testCase "x" $ "x" ~~ 'x'
                , testCase "$" $ "$" ~~ '$'
                , testCase "tab" $ "\t" ~~ '\t'
                ]
            , testGroup "int(eger)"
                [ testCase "1" $ "1" ~~ (1 :: Int)
                , testCase "-2" $ "-2" ~~ (-2 :: Integer)
                , testCase "42" $ "42" ~~ (42 :: Integer)
                ]
            , testGroup "[a]"
                [ testCase "empty" $ "[]" ~~ ([] :: [Bool])
                , testCase "[bool]" $ "[True, False, False]" ~~ [True, False, False]
                , testCase "[char]" $ "\"$&abcdefg&$\"" ~~ "$&abcdefg&$"
                ]
            ]
        , testGroup "operators"
            [ testCase "errS" $
                runParserT (errS "Some error" :: ParserT ()) "any"
                @?= Left (ErrorMsg "Some error")
            , testCase "errE" $
                runParserT (StateT $ const $ errE "Some error" :: ParserT ()) "any"
                @?= Left (ErrorMsg "Some error")
            , testGroup "char"
                [ testCase "succeed" $
                    runParserT (char '#') "#..."
                    @?= Right ('#', "...")
                , testCase "fail" $
                    runParserT (char '#') "$..."
                    @?= Left (ErrorMsg "Expected '#' but got '$'")
                ]
            , testGroup "string"
                [ testCase "succeed" $
                    runParserT (string "someStr") "someStr..."
                    @?= Right ((), "...")
                , testCase "fail" $
                    runParserT (string "someStr") "someX..."
                    @?= Left (ErrorMsg "Expected 'S' but got 'X'")
                ]
            , testGroup "int" $
                let apply op opSymbol = do
                      x <- parser @Int <~ opSymbol
                      y <- parser @Int
                      return $ op x y
                    add = apply (+) "+"
                    sub = apply (-) "-"
                in [ testCase "succeed" $
                      runParserT add "1+2..."
                      @?= Right (3, "...")
                    , testCase "fail" $
                      runParserT add "1-2..."
                      @?= Left (ErrorMsg "Expected '+' but got '-'")
                    , testCase "<|>" $
                      runParserT (add <|> sub) "2-1..."
                      @?= Right (1, "...")
                ]
            , testCase "<|>" $
                runParserT (string "aaa" <|> string "bbb") "bbb..."
                @?= Right ((), "...")
            , testCase "eof" $
                runParserT (string "aaa" <* eof) "aaa..."
                @?= Left (ErrorMsg "Expecting EOF")
            , testGroup "skip"
                [ testCase "succeed" $
                    runParserT (skip (parser @Boolean)) "T..."
                    @?= Right ((), "...")
                , testCase "fail" $
                    runParserT (skip (parser @Boolean)) "X..."
                    @?= Left (ErrorMsg "Expected 'F' but got 'X'")
                ]
            , testGroup "oneOf"
                [ testCase "succeed" $
                    runParserT (oneOf "$%#") "$..."
                    @?= Right ('$', "...")
                , testCase "fail" $
                    runParserT (oneOf "$%#") "a..."
                    @?= Left (ErrorMsg "Expected '#' but got 'a'")
                ]
            , testGroup "??"
                [ testCase "succeed" $
                    runParserT (oneOf "$%#" ??) "$..."
                    @?= Right (['$'], "...")
                , testCase "fail" $
                    runParserT (oneOf "$%#" ??) "^..."
                    @?= Right ([], "^...")
                ]
            , testGroup "many"
                [ testCase "none" $
                    runParserT (oneOf "$%#" ^*) "^..."
                    @?= Right ([], "^...")
                , testCase "single" $
                    runParserT (oneOf "$%#" ^*) "$^..."
                    @?= Right ("$", "^...")
                , testCase "multiple" $
                    runParserT (oneOf "$%#" ^*) "#$#^..."
                    @?= Right ("#$#", "^...")
                ]
            , testGroup "skipMany"
                [ testCase "none" $
                    runParserT (skipMany $ oneOf "$%#") "..."
                    @?= Right ((), "...")
                , testCase "single" $
                    runParserT (skipMany $ oneOf "$%#") "$..."
                    @?= Right ((), "...")
                , testCase "multiple" $
                    runParserT (skipMany $ oneOf "$%#") "#$$..."
                    @?= Right ((), "...")
                ]
            , testCase "space" $
                runParserT (space >> space >> string "end") "  end..."
                @?= Right ((), "...")
            , testGroup "spaces"
                [ testCase "none" $
                    runParserT (spaces >> string "end") "end..."
                    @?= Right ((), "...")
                , testCase "single" $
                    runParserT (spaces >> string "end") " end..."
                    @?= Right ((), "...")
                , testCase "multiple" $
                    runParserT (spaces >> string "end") " \n\t    end..."
                    @?= Right ((), "...")
                , testCase "lexeme" $
                    runParserT (lexeme (parser @Integer)) " \n\t   -1234 \n\t..."
                    @?= Right (-1234, "...")
                , testCase "~>" $
                    runParserT ("##" ~> parser @Integer) " \n\t ## -1234..."
                    @?= Right (-1234, "...")
                , testCase "<~" $
                    runParserT (parser @Integer <~ "##") "-1234 \n\t  ##  ..."
                    @?= Right (-1234, "...")
                ]
            , testCase "enclose" $
                runParserT (enclose "[|" "|]" (parser @Integer)) "[|-1234|]..."
                @?= Right (-1234, "...")
            , testGroup "sepBy (bool)"
                [ testCase "none" $
                    runParserT (parser @Boolean `sepBy` "$") "..."
                    @?= Right ([], "...")
                , testCase "single" $
                    runParserT (parser @Boolean `sepBy` "$") "T..."
                    @?= Right ([T], "...")
                , testCase "multiple" $
                    runParserT (parser @Boolean `sepBy` "$") "T$ F $T$F..."
                    @?= Right ([T,F,T,F], "...")
                ]
            , testGroup "sepBy (int)"
                [ testCase "single" $
                    runParserT (parser @Int `sepBy` "$") "1..."
                    @?= Right ([1], "...")
                , testCase "multiple" $
                    runParserT (parser @Int `sepBy` "$") "1$ 2$3$4 $ 5..."
                    @?= Right ([1..5], "...")
                ]
            ]
        , testGroup "boolean"
            [ testCase "true" $ "T" ~~ T
            , testCase "false" $ "F" ~~ F
            ]
        , testGroup "nat"
            [ testCase "0" $ " Z " ~~ Z
            , testCase "1" $ "  S Z  " ~~ S Z
            , testCase "2" $ " S  (S Z) " ~~ S (S Z)
            ]
        , testGroup "pair"
            [ testCase "(0,0)" $
                " Pair Z Z " ~~ Pair Z Z
            , testCase "(0,1)" $
                " Pair Z (S Z) " ~~ Pair Z (S Z)
            , testCase "(1,0)" $
                " Pair (S Z) Z " ~~ Pair (S Z) Z
            , testCase "(1,1)" $
                " Pair (S Z) (S Z) " ~~ Pair (S Z) (S Z)
            , testCase "(2,1)" $
                " Pair (S (S Z)) (S Z) " ~~ Pair (S (S Z)) (S Z)
            ]
        , testGroup "config"
            [ testCase "default" $
                " Config T -42"
                ~~ Config T (-42 :: Int)
            , testCase "record" $
                " Config { varA = T, varB = -42 } "
                ~~ Config { varA = T, varB = -42 :: Int }
            ]
        , testGroup "taggedList"
            [ testCase "default" $
                " Cons T ( Cons F Nil ) "
                ~~ Cons T (Cons F Nil)
            , testCase "record" $
                "Cons{element=T,rest=Cons F Nil}"
                ~~ Cons { element = T, rest = Cons F Nil }
            ]
        , testGroup "tree"
            [ testCase "depth0" $
                " Leaf " ~~ (Leaf :: Tree Integer)
            , testCase "depth1" $
                "Node Leaf T Leaf" ~~ Node Leaf T Leaf
            , testCase "depth2" $
                "Node (Node Leaf F Leaf) T (Node Leaf F Leaf)"
                ~~ Node (Node Leaf F Leaf) T (Node Leaf F Leaf)
            ]
        , testGroup "roseTree"
            [ testCase "depth0" $
                " RoseLeaf " ~~ (RoseLeaf :: RoseTree Integer)
            , testCase "depth1" $
                "RoseNode T [RoseLeaf, RoseLeaf, RoseLeaf]"
                ~~ RoseNode T [RoseLeaf, RoseLeaf, RoseLeaf]
            , testCase "depth2" $
            "RoseNode T [RoseNode T [RoseLeaf], RoseLeaf, RoseNode F []]"
                ~~ RoseNode T [ RoseNode T [RoseLeaf], RoseLeaf, RoseNode F [] ]
            ]
        , testGroup "pair (infix)"
            [ testCase "(0,0)" $
                " Z :-: Z " ~~ Z :-: Z
            , testCase "(0,1)" $
                " Z :-: (S Z) " ~~ Z :-: S Z
            , testCase "(1,0)" $
                " (S Z) :-: Z " ~~ S Z :-: Z
            , testCase "(1,1)" $
                " (S Z) :-: (S Z) " ~~ S Z :-: S Z
            , testCase "(2,1)" $
                " (S (S Z)) :-: (S Z) " ~~ S (S Z) :-: S Z
            , testCase "((0,1)),(1,0))" $
                " (Z :-: (S Z)) :-: ((S Z) :-: Z) " ~~
                    (Z :-: S Z) :-: (S Z :-: Z)
            , testCase "(0,1),T)" $
                " (Z :-: Z) :-: T " ~~
                    (Z :-: Z) :-: T
            ]
        ]
    ]

infix 1 ~~
(~~) :: (Parse a, Eq a, Show a) => String -> a -> Assertion
s ~~ r = runP (s ++ "...") @?= Right (r, "...")

data Boolean = T | F deriving (Generic, Eq, Show, Parse)
data Nat = Z | S Nat deriving (Generic, Eq, Show, Parse)
data Pair a b = Pair a b deriving (Generic, Eq, Show, Parse)
data InfixPair a b = a :-: b deriving (Generic, Eq, Show, Parse)
data Config a b = Config {varA :: a, varB :: b} deriving (Generic, Eq, Show, Parse)
data TaggedList a = Nil | Cons { element :: a, rest :: TaggedList a }
                    deriving (Generic, Eq, Show, Parse)
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving (Generic, Eq, Show, Parse)
data RoseTree a = RoseLeaf | RoseNode a [RoseTree a]
                  deriving (Generic, Eq, Show, Parse)

readChar :: Parser Char
readChar = Parser $ \case
  (c:cs) -> Right (c, cs)
  _ -> Left $ ErrorMsg "Cannot parse character."
readCharT :: ParserT Char
readCharT = StateT $ \case
  (c:cs) -> return (c, cs)
  _ -> fail "Cannot parse character."

readInt :: Parser Int
readInt = (\c -> read [c]) <$> readChar
readIntT :: ParserT Int
readIntT = (\c -> read [c]) <$> readCharT

readPlus :: Parser ()
readPlus = Parser $ \case
             ('+':cs) -> Right ((), cs)
             _ -> Left $ ErrorMsg "Cannot parse +."
readPlusT :: ParserT ()
readPlusT = StateT $ \case
            ('+':cs) -> return ((), cs)
            _ -> fail "Cannot parse +."
