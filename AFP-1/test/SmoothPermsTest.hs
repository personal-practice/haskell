{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (foldl')
import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set
import System.Environment
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified SmoothPermsSlow as Slow
import qualified SmoothPermsTree as Tree
import qualified SmoothPermsUnfold as Unfold

main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_MAX_SIZE" "10"
  defaultMain $ testGroup "Tests"
    [ testGroup groupName [permTests permsImpl, smoothTests permsImpl smoothImpl]
    | (groupName, permsImpl, smoothImpl) <-
        [ ("SLOW", Slow.perms, Slow.smoothPerms)
        , ("TREE", Tree.perms, Tree.smoothPerms)
        , ("UNFOLD", Unfold.perms, Unfold.smoothPerms)
        ]
    ]

type ImplPerms = [Int] -> [[Int]]
type ImplSmoothPerms = Int -> ImplPerms
type PermProperty p = [Int] -> p

-- | Set the maximum size of QuickCheck for a given property.
(#) :: Int -> TestTree -> TestTree
n # t = localOption (QuickCheckMaxSize n) t

-- | Check whether all permutations preserve some property of the initial list.
allPreserve :: Eq p => ImplPerms -> PermProperty p -> PermProperty Bool
(f `allPreserve` p) l =
  let pInit = p l
  in  and [pInit == p l' | l' <- f l]

allPreserveLength :: ImplPerms -> PermProperty Bool
allPreserveLength f = f `allPreserve` length

allPreserveElems :: ImplPerms -> PermProperty Bool
allPreserveElems f = f `allPreserve` MultiSet.fromList

permTests :: ImplPerms -> TestTree
permTests perms = testGroup "permutations"
  [ testProperty "preserve size" $ allPreserveLength perms
  , testProperty "preserve elements" $ allPreserveElems perms
  , testProperty "have proper cardinality" $ \l ->
      length (perms l) == factorial (length l)
  , 7 # testProperty "form equivalence classes"
      (perms `allPreserve` (Set.fromList . perms))
  ]
  where factorial n = foldl' (*) 1 [1..n]

smoothTests :: ImplPerms -> ImplSmoothPerms -> TestTree
smoothTests perms smoothPerms = testGroup "smooth permutations"
  [ testProperty "preserve size" $ allPreserveLength allSmoothPerms
  , testProperty "preserve elements" $ allPreserveElems allSmoothPerms
  , testProperty "are a subset of permutations" $ \l -> length l > 1 ==>
      let ps = Set.fromList (perms l)
      in  Set.fromList (allSmoothPerms l) `Set.isSubsetOf` ps
  , testProperty "are indeed smooth" $ \l -> length l > 1 ==>
      and [ d <= n
          | n <- nRange
          , l' <- smoothPerms n l
          , d <- distances l'
          ]
  ]
  where allSmoothPerms l = [l' | n <- nRange, l' <- smoothPerms n l]
        distances l = map (\(a, b) -> abs (a - b)) (zip l (tail l))
        nRange = [(-1)..10]
