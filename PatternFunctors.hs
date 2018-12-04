{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module PatternFunctors where

import GHC.Generics

data Tree a = Leaf
            | Node (Tree a) a (Tree a)

data TreeF a t = LeafF
               | NodeF t a t

instance Functor (TreeF a) where
  fmap f LeafF = LeafF
  fmap f (NodeF l x r) = NodeF (f l) x (f r)

-- type TreeS a = (Rec0 (Tree a) :*: Rec0 a :*: Rec0 (Tree a)) :+: U1
-- instance Functor (Rec0 a) => Functor (TreeS a) where
--   fmap f (L1 (l :*: x :*: r)) = L1 ((f <$> l) :*: (f <$> x) :*: (f <$> r))
--   fmap f (R1 _) = R1 U1

