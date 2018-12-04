{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module GADT where

import Prelude hiding (Either (..))

data Tree :: * -> * where
  Leaf :: Tree a
  Node :: Tree a -> a -> Tree a -> Tree a
  deriving (Eq, Show)

data Either :: * -> * -> * where
  Left  :: a -> Either a b
  Right :: b -> Either a b
  deriving (Eq, Show)

{- Expr -}
data Expr :: * -> * where
  LitI   :: Int -> Expr Int
  LitB   :: Bool -> Expr Bool
  IsZero :: Expr Int -> Expr Bool
  Plus   :: Expr Int -> Expr Int -> Expr Int
  If     :: Expr Bool -> Expr a -> Expr a -> Expr a
  Pair   :: Expr a -> Expr b -> Expr (a, b)
  Fst    :: Expr (a, b) -> Expr a
  Snd    :: Expr (a, b) -> Expr b

eval :: Expr a -> a
eval (LitI n)   = n
eval (LitB b)   = b
eval (IsZero n) = eval n == 0
eval (Plus n m) = eval n + eval m
eval (If c l r) = if eval c then eval l else eval r
eval (Pair x y) = (eval x, eval y)
eval (Fst p)    = fst (eval p)
eval (Snd p)    = snd (eval p)

{- Vec -}
data Zero
data Succ a

data Vec :: * -> * -> * where
  Nil  :: Vec a Zero
  Cons :: a -> Vec a n -> Vec a (Succ n)

vhead :: Vec a (Succ n) -> a
vhead (Cons x _) = x

vtail :: Vec a (Succ n) -> Vec a n
vtail (Cons _ xs) = xs

vmap :: (a -> b) -> Vec a n -> Vec b n
vmap _ Nil         = Nil
vmap f (Cons x xs) = Cons (f x) (vmap f xs)

vzip :: Vec a n -> Vec b n -> Vec (a, b) n
vzip Nil Nil                 = Nil
vzip (Cons x xs) (Cons y ys) = Cons (x, y) (vzip xs ys)

snoc :: Vec a n -> a -> Vec a (Succ n)
snoc Nil y         = Cons y Nil
snoc (Cons x xs) y = Cons x (xs `snoc` y)

vreverse :: Vec a n -> Vec a n
vreverse Nil         = Nil
vreverse (Cons x xs) = vreverse xs `snoc` x

data Sum :: * -> * -> * -> * where
  SumZero :: Sum Zero n n
  SumSucc :: Sum m n s -> Sum (Succ m) n (Succ s)

vappend :: Sum n m s -> Vec a n -> Vec a m -> Vec a s
vappend SumZero Nil ys             = ys
vappend (SumSucc p) (Cons x xs) ys = Cons x (vappend p xs ys)

vtoList :: Vec a n -> [a]
vtoList Nil         = []
vtoList (Cons x xs) = x : vtoList xs

data SNat :: * -> * where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

fromList :: SNat n -> [a] -> Vec a n
fromList SZero []         = Nil
fromList (SSucc n) (x:xs) = Cons x (fromList n xs)
fromList _         _      = error "wrong length!"

data VecAnyLen :: * -> * where
  VecAnyLen :: Vec a n -> VecAnyLen a

fromList2 :: [a] -> VecAnyLen a
fromList2 [] = VecAnyLen Nil
fromList2 (x:xs) =
  case fromList2 xs of
    VecAnyLen ys -> VecAnyLen (Cons x ys)

data VecAnyLen' :: * -> * where
  VecAnyLen' :: SNat n -> Vec a n -> VecAnyLen' a

fromList3 :: [a] -> VecAnyLen' a
fromList3 [] = VecAnyLen' SZero Nil
fromList3 (x:xs) =
  case fromList3 xs of
    VecAnyLen' n ys -> VecAnyLen' (SSucc n) (Cons x ys)

{- Equality -}
data Equal :: * -> * -> * where
  Refl :: Equal a a

refl :: Equal a a
refl = Refl
sym :: Equal a b -> Equal b a
sym Refl = Refl
trans :: Equal a b -> Equal b c -> Equal a c
trans Refl Refl = Refl

coerce :: Equal a b -> a -> b
coerce Refl x = x

-- coerceL :: Equal a b -> a -> b
-- coerceL ~Refl x = x
--
-- foo :: Bool -> Int
-- foo b = coerceL undefined b

{- Reflection -}
data Type :: * -> * where
  INT :: Type Int
  BOOL :: Type Bool
  LIST :: Type a -> Type [a]
  PAIR :: Type a -> Type b -> Type (a, b)

data Dynamic :: * where
  Dyn :: Type a -> a -> Dynamic

eqType :: Type a -> Type b -> Maybe (Equal a b)
eqType = undefined

coerceDyn :: Type a -> Dynamic -> Maybe a
coerceDyn t (Dyn t' x) =
  case eqType t t' of
    Just Refl -> Just x
    _         -> Nothing

{- Associated types -}
class Collects c where
  type Elem c
  empty  :: c
  insert :: Elem c -> c -> c
  toList :: c -> [Elem c]

instance Eq e => Collects [e] where
  type Elem [e] = e
  empty = []
  insert x xs = x : xs
  toList = id

{- Addition with MultiParamTypeClasses -}
class Summable n m where
  type TheSum n m
  mkSum :: Sum n m (TheSum n m)

instance Summable Zero n where
  type TheSum Zero m = m
  mkSum = SumZero

instance Summable n m => Summable (Succ n) m where
  type TheSum (Succ n) m = Succ (TheSum n m)
  mkSum = SumSucc mkSum
