module Main where

import Control.Monad
{-
    Monad m =>
        (=<<) :: (a -> m b) -> m a -> m b
        mapM :: (a -> m b) -> [a] -> m [b]
        mapM_ :: (a -> m b) -> [a] -> m ()
        filterM :: (a -> m Bool) -> [a] -> m [a]
        foldM :: (a -> b -> m a) -> a -> [b] -> m a
        sequence :: [m a] -> m [a]
        sequence_ :: [m a] -> m ()
        liftM :: (a -> b) -> m a -> m b
        when :: Bool -> m () -> m ()
        join :: m (m a) -> m a
-}

-- without liftM
numberFile :: FilePath -> IO ()
numberFile fp = do
    text <- readFile fp
    let l = lines text
    let n = zipWith (\n t -> show n ++  ' ':t) [1..] l
    mapM_ putStrLn n
-- with liftM
numberFile2 :: FilePath -> IO ()
numberFile2 fp = do
    l <- lines `liftM` readFile fp
    let n = zipWith (\n t -> show n ++ ' ':t) [1..] l
    mapM_ putStrLn n


main =
    {-writeFile "foo" "hello world!" >>
        (readFile "foo" >>= putStrLn)-}
    {-writeFile "foo" "hello world!" >>
        (putStrLn =<< readFile "foo")-}

    -- mapM_ print [1, 2, 3, 4 ,5]

    {-foldM (\a b ->
            putStrLn (show a ++ "+" ++
                    show b ++ "=" ++ show (a+b)) >>
            return (a + b)) 0 [1..5]-}

    -- sequence [print 1, print 2, print 'a']
    -- sequence_ [print 1, print 2, print 'a']

    -- numberFile "foo"
    -- numberFile2 "foo"
    -- (map words . lines) `liftM` readFile "foo"

    -- return $ liftM (+1) (Just 5)
    -- return $ liftM (+1) Nothing

    {-mapM_ (\l -> when (not $ null l) (putStrLn l))
        ["", "abc","def","","ghc"]-}

    -- return $ join (Just (Just 'a'))
    -- return $ join (Just (Nothing :: Maybe Char))
    -- return $ join (Nothing :: Maybe (Maybe Char))
    -- join (return (putStrLn "hello"))
    -- return (putStrLn "hello")
    -- return $ join [[1, 2, 3], [4, 5]]



