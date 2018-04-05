module SmoothPermsSlow (perms, smoothPerms) where

-- | Extract all possible elements from a list.
split :: [a] -> [(a, [a])]
split []     = []
split (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- split xs]

-- | Returns all permutations of a list.
perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = [v:p | (v, vs) <- split xs, p <- perms vs]

-- | Check whether a permutation respects the given max distance.
smooth :: Int -> [Int] -> Bool
smooth n (x:y:ys) = abs (y - x) <= n && smooth n (y:ys)
smooth _ _        = True

-- | Returns all /smooth permutations/ of a list with a given maximum distance.
smoothPerms :: Int     -- ^ the maximum distance to respect
            -> [Int]   -- ^ the initial list
            -> [[Int]] -- ^ the smooth permutations
smoothPerms n xs = filter (smooth n) (perms xs)
