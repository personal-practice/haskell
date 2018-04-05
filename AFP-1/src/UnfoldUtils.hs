module UnfoldUtils () where

unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr next x = case next x of
                   Nothing     -> []
                   Just (y, r) -> y : unfoldr next r

data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving Show

unfoldTree :: (s -> Either a (s, s)) -> s -> Tree a
unfoldTree next x = case next x of
                      Left  y      -> Leaf y
                      Right (l, r) -> Node (unfoldTree next l) (unfoldTree next r)

iterate :: (a -> a) -> a -> [a]
iterate f = unfoldr (\y -> Just (y, f y))

{-
=======================================
PROOF: forall l f. map f l == MAP f l
=======================================

    BASE CASE:
      { l == [] } ==>
        map f l == MAP f l
      ~ map f l == MAP f []
      ~ map f l == []
    -------------------------------------------------------
       map f l
    == map f []
    == unfoldr next []
    == case next [] of
        Nothing -> []
        Just ... -> ...
    == case Nothing of
        Nothing -> []
        Just ... -> ...
    == []

    INDUCTIVE STEP:
      { l == x:l' /\ map f l' == MAP f l' } ==>
        map f l == MAP f l
      ~ map f l == MAP f (x:l')
      ~ map f l == f x : MAP f l
    ---------------------------------------------------------
       map f l
    == map f (x:l')
    == unfoldr next (x:l')
    == case next (x:l') of
        Nothing -> ...
        Just (y, r) -> y : unfolder next r
    == case Just (f x, l') of
        Nothing -> ...
        Just (y, r) -> y : unfolder next r
    == f x : unfolder next l'
    == f x : map f l'
    == f x : MAP f l'
-}
map :: (a -> b) -> [a] -> [b]
map f = unfoldr next
  where next []     = Nothing
        next (x:xs) = Just (f x, xs)

{-
==============================================
PROOF: forall n. size (balanced n) == 2^n - 1
==============================================
{ size (Leaf _)   = 0
  size (Node l r) = 1 + size l + size r
}

    BASE CASE:
      { n == 0 } ==>
        size (balanced 0) == 2^0 - 1
      ~ size (balanced 0) == 0
    -------------------------------------------------------
       size (balanced 0)
    == size (unfoldTree next 0)
    == size (case next 0 of
        Left y -> Leaf y
        Right ... -> ...)
    == size (case Leaf ()
        Left y -> Leaf y
        Right ... -> ...)
    == size (Leaf ())
    == 0

    INDUCTIVE STEP:
      { n == n' + 1 /\ size (balanced n') == 2^n' - 1 } ==>
        size (balanced n) == 2^n - 1
    ---------------------------------------------------------
       size (balanced n)
    == size (balanced (n + 1))
    == size (case next (n + 1) of
        Left ... -> ...
        Right (l, r) -> Node (unfoldTree next l) (unfoldTree next r))
    == size (case Right (n - 1, n - 1) of
        Left ... -> ...
        Right (l, r) -> Node (unfoldTree next l) (unfoldTree next r))
    == size (Node (unfoldTree next (n - 1)) (unfoldTree next (n - 1))))
    == size (Node (unfoldTree next n') (unfoldTree next n'))
    == size (Node (balanced n') (balanced n'))
    == 1 + size (balanced n') + size (balanced n')
    == 1 + (2^n' - 1) + (2^n' - 1)
    == 1 + 2*2^n' - 2
    == 2^(n' + 1) - 1
    == 2^n - 1
-}
balanced :: Int -> Tree ()
balanced = unfoldTree next
  where next 0 = Left ()
        next n = Right (n - 1, n - 1)

sized :: Int -> Tree Int
sized n = unfoldTree next (n, 0)
  where next (0, offset) = Left offset
        next (k, offset) = Right ((k-1, offset), (0, offset + k))
