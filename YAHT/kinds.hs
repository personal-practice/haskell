module Main where

main :: IO ()
main = putStrLn "main"

data Maybe a =
    Just a
    | Nothing

data Pair a b = Pair a b

-- :: (* -> *) -> * -> * -> *
data Strance c a b =
    MkStrange (c a) (c b)

-- :: * -> * -> *
type MaybePair = Strange Maybe

type MaybePair1 = Strange Maybe
type MaybePair2 a = Strange Maybe a
type MaybePair3 a b = Strange Maybe ab

-- Valid
type MaybePair1a = MaybePair1
type MaybePair1b = MaybePair1 Int
type MaybePair1c = MaybePair1 Int Double
type MaybePair2b = MaybePair2 Int
type MaybePair2c = MaybePair2 Int Double
type MaybePair3c = MaybePair3 Int Double

-- Invalid
type MaybePair2a = MaybePair2
type MaybePair3a = MaybePair3
type MaybePair3b = MaybePair3 Int
