module Main where

import System.Environment

-- type List3D = [(Double, Double, Double)]

-- DOESN'T COMPILE!!
-- type BadType = Int -> BadType

type List3D a = [(a, a,a)]

newtype MyInt = MyInt Int

-- DOESN'T COMPILE!!
-- newtype Bad1 = Bad1a Int | Bad1b Double
-- newtype Bad2 = Bad2 Int Double

{-instance Ord MyInt where
    MyInt i < MyInt j
        | odd i && odd j = i < j
        | even i && even j = i < j
        | even i = True
        | otherwise = False
        where odd x = (x `mod` 2) == 0
                   even = not . odd-}

-- mkMyInt i = MyInt i
-- unMyInt (MyInt i) = i

data Unit = Unit

-- Strict fields
data SMaybe a =
    SNothing
    |  SJust !a
    deriving Show

main :: IO()
main = do
    [cmd] <- getArgs
    case cmd of
        "a" -> printJust undefined
        "b" -> printJust Nothing
        "c" -> printJust (Just undefined)
        "d" -> printJust (Just ())

        "e" -> printSJust undefined
        "f" -> printSJust SNothing
        "g" -> printSJust (SJust undefined)
        "h" -> printSJust (SJust ())

printJust :: Maybe () -> IO ()
printJust Nothing = putStrLn "Nothing"
printJust (Just x) = do putStr "Just "; print x

printSJust :: SMaybe () -> IO ()
printSJust SNothing = putStrLn "Nothing"
printSJust (SJust x) = do putStr "Just "; print x
