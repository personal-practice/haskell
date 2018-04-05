{-# OPTIONS_GHC -O0 #-}
module Fix where

fix :: (a -> a) -> a
fix f = f (fix f)

-------------------------------- Question 1 ------------------------------------
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = fix $ \foldr' f z l ->
          case l of
            []     -> z
            (x:xs) -> f x (foldr' f z xs)

-------------------------------- Question 2 ------------------------------------

-- y f = (\x -> f (x x)) (\x -> f (x x))
-- *** Occurs check: cannot construct the infinite type: t0 ~ t0 -> t
--     Expected type: t0 -> t
--     Actual type: (t0 -> t) -> t
--
-- The error happens in the expression `(x x)`.
-- Since x is applied to some argument, its type must be `a -> b`.
-- But x is also the argument, hence `a ~ a -> b`.
-- This leads to a failure in the unification process. Its name, /occurs-check/,
-- comes from the fact that the variable `a`, which we try to instantiate, occurs
-- on the right-hand side, which is not allowed. The term /occurs-check/ is taken
-- from a logic-programming background, since unification plays a dominant role
-- in the semantics of logic programs.

data F a = F { unF :: F a -> a }

-- To compile this code, GHC requires disabling optimizations and defining 'F'
-- as data instead of newtype, otherwise you would get the following error:
-- *** GHC Panic! Simplifier ticks exhausted...
y :: (t -> t) -> t
y f = (\x -> f (unF x x)) $ F (\x -> f (unF x x))
