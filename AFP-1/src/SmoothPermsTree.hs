module SmoothPermsTree (perms, smoothPerms) where

newtype PermTree a = PermTree [(a, PermTree a)]

-- | Create the `PermTree` of a given list.
listToPermTree :: [a] -> PermTree a
listToPermTree [] = PermTree []
listToPermTree l =
  PermTree [(v, listToPermTree vs) | (v, vs) <- split l]
  where split []     = []
        split (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- split xs]

-- | Generate all permutations from a `PermTree`.
permTreeToPerms :: PermTree a -> [[a]]
permTreeToPerms (PermTree []) = [[]]
permTreeToPerms (PermTree ps) =
  concatMap (\(v, ps') -> map (\l -> v : l) $ permTreeToPerms ps') ps

-- | Returns all permutations of a list.
perms :: [a] -> [[a]]
perms = permTreeToPerms . listToPermTree

-- | Prune sub-trees that do not respect the given max distance.
pruneSmooth :: Int -> PermTree Int -> PermTree Int
pruneSmooth _ (PermTree []) = PermTree []
pruneSmooth n (PermTree ps) = PermTree $ map (pruneSmooth' n) ps

pruneSmooth' :: Int -> (Int, PermTree Int) -> (Int, PermTree Int)
pruneSmooth' _ p@(_, PermTree []) = p
pruneSmooth' n (v, PermTree ps) = (v, PermTree pruned)
  where pruned = map (pruneSmooth' n) (filter (\(v', _) -> abs (v - v') <= n) ps)

-- | Returns all /smooth permutations/ of a list with a given maximum distance.
--   __NOTE__: Posthumously filters out pruned permutations. This leads to worse
--   performance, but greatly aids readability of the code above, since there is
--   no need to separate leafs from pruned nodes (both are PermTree []).
smoothPerms :: Int     -- ^ the maximum distance to respect
            -> [Int]   -- ^ the initial list
            -> [[Int]] -- ^ the smooth permutations
smoothPerms n xs = filter (\l -> length l == length xs)
                 $ permTreeToPerms
                 $ pruneSmooth n
                 $ listToPermTree xs
