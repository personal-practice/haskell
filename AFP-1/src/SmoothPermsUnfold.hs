module SmoothPermsUnfold (perms, smoothPerms) where

newtype PermTree a = PermTree [(a, PermTree a)] deriving (Eq, Show)

-- Unfold generator for `PermTree`.
unfoldPermTree :: (s -> [(a, s)]) -> s -> PermTree a
unfoldPermTree next x =
  case next x of
    [] -> PermTree []
    rs -> PermTree [(a, unfoldPermTree next s) | (a, s) <- rs]

-- | Create the `PermTree` of a given list.
listToPermTree :: [a] -> PermTree a
listToPermTree = unfoldPermTree next
  where next [] = []
        next rs = split rs
        split []     = []
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
--   (also implemented with unfold for masochistic purposes)
pruneSmooth :: Int -> PermTree Int -> PermTree Int
pruneSmooth n tree = unfoldPermTree next (const True, tree)
  where next :: (Int -> Bool, PermTree Int) -> [(Int, (Int -> Bool, PermTree Int))]
        next (_, PermTree []) = []
        next (f, PermTree rs) =
          [(a, (\x -> abs (x - a) <= n, subTree)) | (a, subTree) <- rs, f a]

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
