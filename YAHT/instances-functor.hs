module Main where

main :: IO ()
main = putStrLn "main"

class Functor f where
    fmap :: (a -> b) -> f a -> f b

data BinTree a =
    Leaf a
    | Branch (BinTree a) (BinTree a)

instance Functor BinTree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Branch left right) =
        Branch (fmap f left) (fmap f right)

instance Eq a => Eq (BinTree a) where
    Leaf a == Leaf b = a == b
    Branch l r == Branch l' r' =
        l == l' && r == r'
    _ == _ = False


-- Closed-world assumption
class MyEq a where
    myeq :: a -> a -> Bool

instance Eq a => MyEq a where
    myeq = (==)

instance MyEq a => Eq a where
    (==) = myeq

--

class OnlyInts a where
    foo :: a -> a -> Bool

instance OnlyInts Int where
    foo = (==)

bar :: OnlyInts a => a -> bool
bar = foo 5
