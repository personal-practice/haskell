module Main where

import Data.Graph as Graph
-- List monad
{-instance Monad [] where
    return x = [x]
    l >>= f = concatMap f l
    fail _ = []-}

-- Maybe monad
{-instance Monad Maybe where
    return a                =  Just a
    Nothing >>= f    = Nothing
    Just x >>= f        = f x
    fail _                     = Nothing-}

cross l1 l2 = do
    x <- l1
    y <- l2
    return (x,  y)

cross2 l1 l2 =
    [(x, y) | x <- l1, y <- l2]


searchAll :: Monad m => Graph v e -> Int -> Int -> m [Int]
searchAll g@(Graph vl el) src dst
    | src == dst = return [src]
    | otherwise = search' el
    where
        search' [] = fail "no path"
        search' ((u, v, _):es)
            | src == u =
                searchAll g v dst >>=
                    \path -> return (u:path)
            | otherwise = search' es



main =
    return $ show $
        cross "ab" "123"
        -- cross2 "ab" "123"
        -- cross (Just 'a') (Just 'b')
        -- cross (Nothing :: Maybe Char) (Just 'b')
        -- cross (Just 'a') (Nothing :: Maybe Char)
        -- cross (Nothing :: Maybe Char) (Nothing :: Maybe Char)
        -- searchAll gr 0 3 :: Maybe [Int] --> [0,1,3]
        -- searchAll gr 3 0 :: Maybe [Int] --> Nothing
        -- searchAll gr 0 3 :: [[Int]] --> [[1,2,3]]
        -- searchAll gr 3 0 :: [[Int]] --> []
        -- searchAll gr 0 3 :: IO [Int] --> [0,1,3]
        -- searchAll gr 3 0 :: IO [Int] --> Exception: 'no path'

gr = Graph [(0, ’a’), (1, ’b’), (2, ’c’), (3, ’d’)] [(0,1,’l’), (0,2,’m’), (1,3,’n’), (2,3,’m’)]
