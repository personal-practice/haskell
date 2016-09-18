module Main
( Point (..) -- = Point(Point)
, Shape (..) -- = Shape(Circle, Rectangle)
, surface
, nudge
, main
) where

import Data.Map hiding (singleton)

main = putStrLn "main"

{- Data types -}
data Boolean =  False | True

data Point = Point Float Float deriving Show

data Shape =
    Circle Point Float
    | Rectangle Point Point
    deriving Show

surface :: Shape -> Float
surface (Circle _ r) =
    pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) =
    (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b =
    Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
    Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

{- Records -}
data Person = Person { firstName :: String
                                    , lastName :: String
                                    , age :: Int
                                    , height :: Float
                                    , phoneNumber :: String
                                    , flavor :: String
                                    } deriving (Show)

{- Type patameters -}
data Maybe2 a = Nothing2 | Just2 a

-- Note: never add typeclass constraints in data declarations
data Vector a = Vector a a a deriving Show

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

{- Derived instances -}
data Day = Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday
                    deriving (Eq, Ord, Show, Read, Bounded, Enum)

{- Type synonyms -}
type String2 = [Char]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssocList k v = [(k, v)]

type IntMap = Map Int -- short
type IntMap2 v = Map Int v -- long

-- School lockers example
data Either2 a b = Left2 a| Right2 b deriving (Eq, Ord, Read, Show)
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Data.Map.lookup lockerNumber map of
        Nothing -> Left $ show lockerNumber ++ " does not exist!"
        Just (state, code) ->
            if  state /= Taken
                then Right code
                else Left $ show lockerNumber ++ " already taken!"

{- Recursive data structures -}
data List2 a = Empty2 | Cons a (List2 a) deriving (Show, Read, Eq, Ord)

-- automatic infix operators
infixr 5 :-:
data List3 a = Empty | a :-: (List3 a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List3 a -> List3 a -> List3 a
Empty .++ ys = ys
(x:-:xs) .++ ys = x :-: (xs .++ ys)

-- Binary search tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a)
                        deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a    = Node a (treeInsert a left) right
    | x > a    = Node a left (treeInsert a right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right
