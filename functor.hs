module Main where

import Prelude hiding Functor

main = putStrLn "main"

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap = map

instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x lhs rhs) =
        Node (f x) (fmap f lhs) (fmap f rhs)

instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x

instance Functor ((->) r) where
    fmap = (.)
    -- fmap f g = (\x -> f (g x))

{-
    Applicative Functors
-}

-- pure (+) <*> Just 3 <*> Just 5

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

-- (++) <$> Just "john" <*> Just "travolta"

-- Applicative List
instance Applicative [] where
    pure x = [x]
    fs <*> xs [f x | f <- fs, x <- xs]

instance  Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

-- Applicate IO
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return $ f x

-- Applicative Functions
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)


