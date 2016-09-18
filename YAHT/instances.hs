module Main where

data Color
    = Red
    |   Orange
    | Yellow
    | Green
    | Blue
    | Purple
    | White
    | Black
    | Custom Int Int Int
    deriving (Show,Eq)


instance Eq Color where
    Red == Red = True
    Orange == Orange = True
    Yellow == Yellow = True
    Green == Green = True
    Blue == Blue = True
    Purple == Purple = True
    White == White = True
    Black == Black = True
    (Custom r g b) == (Custom r g' b') =
        r == r' && g == g' && b == b'
    _ == _ = False

instance Show Color where
    show Red = "Red"
    show Orange = "Orange"
    show Yellow = "Yellow"
    show Green = "Green"
    show Blue = "Blue"
    show Purple = "Purple"
    show White = "White"
    show Black = "Black"
    show (Custom r g b) =
        "Custom " ++ show r  ++ " " ++ show g  ++ " "  ++ show b

data Maybe a = Nothing
                        |   Just a
                        deriving (Eq, Ord, Show, Read)

instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True
    (Just x) == (Just x') =
        x == x'
