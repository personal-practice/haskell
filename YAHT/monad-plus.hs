module Main where

import Control.Monad

data Graph v e = Graph [(Int,v)] [(Int,Int,e)]

{-class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus Maybe where
    mzero = Nothing
    mplus Nothing y = y
    mplus x _ = x

instance MonadPlus [] where
    mzero = []
    mplus = (++)-}

searchAll2 g@(Graph vl el) src dst
    | src == dst = return [src]
    | otherwise =
            search' el
            where
                search' [] = fail "no path"
                search' ((u,v,_):es)
                    | src == u =
                        (searchAll2 g v dst >>= \path ->
                         return (u:path)) `mplus`
                        search' es
                    | otherwise = search' es

main :: IO ()
main = concatM $ searchAll2 gr 0 3 :: [[Int]]

gr = Graph [(0, 'a'), (1, 'b'), (2, 'c'), (3, 'd')] [(0,1,'l'), (0,2,'m'), (1,3,'n'), (2,3,'m')]
