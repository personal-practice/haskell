-- :set -package syb
{-# LANGUAGE DeriveDataTypeable #-}
module G where

import Data.Data
import Data.Generics.Schemes

data X l = Xm (Maybe Int) (X l)
         | Xs String (X l)
         | Xy (Y l)
         | Xann l
         | ENDx
         deriving (Eq, Show, Typeable, Data)

data Y l = Ym (Maybe Int) (Y l)
         | Ys String (Y l)
         | Yx (X l)
         | Yann l
         | ENDy
         deriving (Eq, Show, Typeable, Data)


{-
listify :: Typeable r => (r -> Bool) -> GenericQ [r] 

-}

g :: Data l => X l -> [Maybe Int]
g = listify (const True)
  -- where
  --   p :: Maybe Int -> Bool
  --   p 

ex :: X ()
ex = Xs "A"
   $ Xm Nothing 
   $ Xm (Just 1)
   $ Xs "B"
   $ Xm (Just 2)
   $ Xm (Nothing)
     $ Xy 
     $ Ys "a"
     $ Ym Nothing
     $ Ym (Just 1)
     $ Ys "b"
     $ Ym (Just 2)
     $ Ym Nothing
     $ Yx
   $ Xs "C"
   $ Xm Nothing
   $ Xm (Just 3)
   $ ENDx

main :: IO ()
main = print $ g ex
