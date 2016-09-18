module Main where

-- a >> x = a >>= \_ -> x
{-class Monad m where
    return :: a -> m a
    fail :: String -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b-}

class Computation c where
    success :: a -> c a
    failure :: String -> c a
    augment :: c a -> (a -> c b) -> c b

f :: String -> String
f str = str

-- do notation
main :: IO ()
main = do
    s <- readFile "somefile"
    putStrLn (show (f s))
{-main =
    -- readFile "somefile" `augment` \s -> putStrLn (show (f s))
    readFile "somefile" >>= \s -> putStrLn (show (f s))-}

{-
    Do Translation Rules
    ============
    1. do {e} -> e
    2. do {e; es} → e >> do {es}
    3. do {let decls; es} → let decls in do {es}
    4. do {p <- e; es} → let ok p = do {es} ; ok _ = fail "....."
        in e >>= ok

    Monad Laws
    ========
    1. return a >>= f ≡ f a
        e.g.  law1a = do
                    x <- return a
                    f x
                law1b = do
                    f a
    2. f >>= return ≡ f
        e.g. law2a = do
                    x <- f
                    return x
                law2b = do
                    f
    3. f >>= (\x -> g x >>= h) ≡ (f >>= g) >>= h
    OR f >>= (g >>= h) ≡ (f >>= g) >>= h
        e.g. law3a = do
                    x <- f
                    do g <- x
                         h y
                law3b = do
                    y <- do x <- f
                                 g x
                    h y
-}
