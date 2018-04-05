{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
module Nested where

type Square = Square' Nil -- note that it is eta-reduced
data Square' t a = Zero (t (t a))
                 | Succ (Square' (Cons t) a)

data Nil    a = Nil deriving Show
data Cons t a = Cons a (t a) deriving Show

-------------------------------- Question 1 ------------------------------------

-- ( (1 0) (0 1) )
square2 :: Square Int
square2 = Succ $ Succ $ Zero $ Cons r10 $ Cons r01 Nil
  where r10 = Cons 1 $ Cons 0 Nil
        r01 = Cons 0 $ Cons 1 Nil

-- ( (1 2 3) (4 5 6) (7 8 9) )
square3 :: Square Int
square3 = Succ $ Succ $ Succ $ Zero $ Cons r123 $ Cons r456 $ Cons r789 Nil
  where r123 = Cons 1 $ Cons 2 $ Cons 3 Nil
        r456 = Cons 4 $ Cons 5 $ Cons 6 Nil
        r789 = Cons 7 $ Cons 8 $ Cons 9 Nil

-------------------------------- Question 2 ------------------------------------
eqNil :: (a -> a -> Bool)
      -> (Nil a -> Nil a -> Bool)
eqNil _ Nil Nil = True

eqCons :: (forall b. (b -> b -> Bool) -> (t b -> t b -> Bool))
       -> (a -> a -> Bool)
       -> (Cons t a -> Cons t a -> Bool)
eqCons eqT eqA (Cons x xs) (Cons y ys) = eqA x y && eqT eqA xs ys

-- OMITTING FORALL
-- ~~~~~~~~~~~~~~~
-- eqCons :: ((b -> b -> Bool) -> (t b -> t b -> Bool))
--        -> (a -> a -> Bool)
--        -> (Cons t a -> Cons t a -> Bool)
-- eqCons eqT eqA (Cons x xs) (Cons y ys) = eqA x y && eqT eqA xs ys
--
-- *** Couldn't match type ‘b’ with ‘a’
--     ‘b’ is a rigid type variable bound by
--     the type signature for:
--       eqCons :: forall b (t :: * -> *) a.
--                  ((b -> b -> Bool) -> t b -> t b -> Bool)
--                  -> (a -> a -> Bool) -> Cons t a -> Cons t a -> Bool
--
-- Since the `forall` moved to the outer scope, it is the caller of the function
-- that will provide the type variables `a` and `b`. Since we have the expression
-- `eqT eqA`, `a` must unify with `b`. But the caller might choose any `a` and
-- `b` he wants! Hence, the type checker cannot make sure this is a well-typed
-- program.
--
-- NOTE: In logical terms, treating `forall` as universal quantification and
-- arrow as implication, moving `forall` in the outer scope leads to a different
-- logical formula, i.e.
--
-- `(forall x. P) => Q`
--  is equivalent to
-- `exists x. P => Q`
--  which is not equivalent to
-- `forall x. P => Q`
--
-- Of course, we could immediately replace `b` with `a`, since we know we will
-- only apply `eqT` to `eqA`.

-------------------------------- Question 3 ------------------------------------
eqSquare' :: (forall b. (b -> b -> Bool) -> (t b -> t b -> Bool))
          -> (a -> a -> Bool)
          -> (Square' t a -> Square' t a -> Bool)
eqSquare' eqT eqA (Zero xs) (Zero ys) = eqT (eqT eqA) xs ys
eqSquare' eqT eqA (Succ xs) (Succ ys) = eqSquare' (eqCons eqT) eqA xs ys
eqSquare' _ _ _ _                     = False

-- OMITTING FORALL
-- ~~~~~~~~~~~~~~~
-- This has the same problems with the `eqCons` case. Moreover, we now cannot
-- fallback to immediately instantiating `b` with `a`, since we apply `eqT` both
-- to `a` and `t a` (due to the nested application in the `Zero` case).
--
-- Thus, we now really need the freedom to choose an appropriate type for certain
-- specific applications.

eqSquare :: (a -> a -> Bool)
         -> (Square a -> Square a -> Bool)
eqSquare = eqSquare' eqNil

instance Eq a => Eq (Square a) where
  (==) = eqSquare (==)

-------------------------------- Question 4 ------------------------------------
type Mapper f a b = (a -> b) -> f a -> f b

mapNil :: Mapper Nil a b
mapNil _ Nil = Nil

mapCons :: (forall x y. Mapper t x y) -> Mapper (Cons t) a b
mapCons mapT mapA (Cons x xs) = Cons (mapA x) (mapT mapA xs)

mapSquare' :: (forall x y. Mapper t x y) -> Mapper (Square' t) a b
mapSquare' mapT mapA (Zero xs) = Zero $ mapT (mapT mapA) xs
mapSquare' mapT mapA (Succ xs) = Succ $ mapSquare' (mapCons mapT) mapA xs

mapSquare :: Mapper Square a b
mapSquare = mapSquare' mapNil

instance Functor Square where
  fmap = mapSquare

-------------------------------- Question 5 ------------------------------------
-- Assume you have the following /type-synonym/:
-- > type P a = (a, a)

-- If partially-applied type synonyms were allowed, we could write a 'Functor'
-- instance for it, as such:
-- > instance Functor P where
-- >   fmap f (x, y) = (f x, f y)

-- Now, consider the predefined 'Functor ((,) a)':
-- > instance Functor ((,) a) where
-- >   fmap f (x,y) = (x, f y)

-- Now assume a term like:
-- > (+ 1) <$> (1, 2)

-- If we take '<$>' of 'P', this would evaluate to (2, 3)
-- But if we took the predefined '<$>', we would get (1, 3).

-- Hence, although type synonyms declare that two types are equal
-- (i.e. interchangable), it is not the case in the above example, since we get
-- a different result depending on which of the two types we infer.
