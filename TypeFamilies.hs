{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- {-# LANGUAGE TypeApplications      #-}
module TypeFamilies where

import Prelude hiding (Bool (..), read)

data Nat = Zero | Succ Nat

{- Addition with TypeFamilies -}
type family Sum n m where
  Sum Zero n = n
  Sum (Succ n) m = Succ (Sum m n)

data Bool = True | False

type family Not (b :: Bool) :: Bool where
  Not True  = False
  Not False = True

{- Member: Typeclasses -}
class Member x xs
instance {-# OVERLAPS #-} Member x (x ': xs)
instance {-# OVERLAPPABLE #-} Member x xs => Member x (y ': xs)

testMem :: (Show a, Member a [Int, Bool, Maybe String]) => a -> IO ()
testMem = print

{- Member: Type-families -}
type family Member'' (x :: k) (xs :: [k]) :: Bool where
  Member'' x '[] = 'False
  Member'' x (x ': xs) = 'True
  Member'' x (y ': xs) = Member'' x xs

type Member' x xs = Member'' x xs ~ True

testMem' :: (Show a, Member' a [Int, Bool, Maybe String]) => a -> IO ()
testMem' = print

{- Type-level appplication -}
newtype Apply f a = MkApply (f a)
-- > :set -XPolyKinds
-- > :info Apply
-- type role Apply representational nominal
-- data Apply (f :: k -> *) (a :: k) = MkApply (f a)

{- Proxy types -}
read :: Read a => String -> a
read = undefined

testRead = read "123" :: Int

read' :: Read a => a -> String -> a
read' = undefined

testRead' = read' (undefined :: Int) "123"

data Proxy (a :: k) = Proxy

read'' :: Read a => Proxy a -> String -> a
read'' _ = undefined

testRead'' = read'' (Proxy :: Proxy Int) "123"

-- testRead''' = read @Int "123"
