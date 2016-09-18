module Main where

main :: IO ()
main = putStrLn "main"


data Graph v e = Graph [(Int, v)] [(Int, Int, e)]

-- Simple
search :: Graph v e -> Int -> Int -> Maybe [Int]
search g@(Graph vl el) src dst
    | src == dst = Just [src]
    | otherwise = search' el
    where
        search' [] = Nothing
        search' ((u,v,_):es)
            | src ==  u =
                case search g v dst of
                    Just p -> Just (u:p)
                    Nothing -> search' es
            | otherwise = search' es

-- With error messages
data Failable a = Success a | Fail String

search2 :: Graph v e -> Int -> Int -> Failable [Int]
search2 g@(Graph vl el) src dst
    | src == dst = Success [src]
    | otherwise = search' el
    where
        search' [] = Fail "No path"
        search' ((u,v,_):es)
                | src == u =
                    case search2 g v dst of
                        Success p -> Success (u:p)
                        _ -> search' es
                | otherwise = search' es

-- All possible paths
search3 :: Graph v e -> Int -> Int -> [[Int]]
search3 g@(Graph vl el) src dst
    | src == dst = [[src]]
    | otherwise = search' el
    where
        search' [] = []
        search' ((u,v,_):es)
                | src == u =
                    map (u:) (search3 g v dst) ++ search' es
                | otherwise = search' es


-- Generalize
class Computation c where
    success :: a -> c a
    failure :: String -> c a
    augment :: c a -> (a -> c b) -> c b
    combine :: c a -> c a -> c a

instance Computation Maybe where
    success = Just
    failure = const Nothing
    augment (Just x) f = f x
    augment Nothing _ = Nothing
    combine Nothing y = y
    combine x _ = x

instance Computation Failable where
    success = Success
    failure = Fail
    augment (Success x) f = f x
    augment (Fail s) _ = Fail s
    combine (Fail _) y = y
    combine x _ = x

instance Computation [] where
    success a = [a]
    failure = const []
    augment l f = concat (map f l)
    combine = (++)

searchAll g@(Graph vl el) src dst
    | src == dst = success [src]
    | otherwise = search' el
    where
        search' [] = failure "no path"
        search' ((u,v,_):es)
            | src == u =
                (searchAll g v dst `augment`
                (success . (u:)))
                `combine` search' es
            | otherwise = search' es
