module Datatypes where

import Control.Monad ((>=>))
import Data.Map      hiding (lookup, mapMaybe)
import Data.Maybe
import Data.Monoid   ((<>))
import Prelude       hiding (lookup)

{-------------------------------------------------------------------------------
                            Tree
-------------------------------------------------------------------------------}
data Tree a = Leaf a
            | Node (Tree a) (Tree a)

instance Functor Tree where
  fmap f (Leaf x)   = Leaf (f x)
  fmap f (Node l r) = Node (f <$> l) (f <$> r)

instance Applicative Tree where
  pure = Leaf

  Leaf f     <*> n = f <$> n
  Node fl fr <*> n = Node (fl <*> n) (fr <*> n)

  -- Zippy version (not compatible with 'Monad')
  -- Node fl fr <*> Leaf x     = ($ x) <$> n
  -- Node fl fr <*> Node xl xr = Node (fl <*> xl) (fr <*> xr)

instance Monad Tree where
  return = Leaf

  (Leaf x)   >>= k = k x
  (Node l r) >>= k = Node (l >>= k) (r >>= k)

instance Foldable Tree where
  foldMap f (Leaf x)   = f x
  foldMap f (Node l r) = foldMap f l <> foldMap f r

instance Traversable Tree where
  traverse f (Leaf x)   = Leaf <$> f x
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

{-------------------------------------------------------------------------------
                            RoseTree
-------------------------------------------------------------------------------}
data RoseTree a = RoseNode a [RoseTree a]
                | RoseLeaf

instance Functor RoseTree where
  fmap f (RoseNode x xs) = RoseNode (f x) ((f <$>) <$> xs)
  fmap _ RoseLeaf        = RoseLeaf

instance Applicative RoseTree where
  pure x = RoseNode x []

  RoseLeaf        <*> _               = RoseLeaf
  _               <*> RoseLeaf        = RoseLeaf
  (RoseNode f []) <*> n               = f <$> n
  (RoseNode f fs) <*> n =
    case f <$> n of
      RoseLeaf      -> RoseLeaf
      RoseNode b bs -> RoseNode b (bs ++ [g <*> n | g <- fs])

  -- Zippy version (not compatible with 'Monad')
  -- t               <*> (RoseNode x []) = ($ x) <$> t
  -- (RoseNode f fs) <*> (RoseNode x xs) =
  --   RoseNode (f x) [g <*> y | (g, y) <- zip fs xs]

instance Monad RoseTree where
  return x = RoseNode x []

  RoseLeaf        >>= _ = RoseLeaf
  (RoseNode x []) >>= k = k x
  (RoseNode x xs) >>= f =
    case f x of
      RoseLeaf      -> RoseLeaf
      RoseNode y ys -> RoseNode y (ys ++ [x' >>= f | x' <- xs])

instance Foldable RoseTree where
  foldMap _ RoseLeaf        = mempty
  foldMap f (RoseNode x []) = f x
  foldMap f (RoseNode x xs) = f x <> foldl1 (<>) [foldMap f x' | x' <- xs]

instance Traversable RoseTree where
  traverse _ RoseLeaf        = pure RoseLeaf
  traverse f (RoseNode x []) = flip RoseNode [] <$> f x
  traverse f (RoseNode x xs) =
    RoseNode <$> f x <*> sequenceA [traverse f x' | x' <- xs]

{-------------------------------------------------------------------------------
                            Teletype
-------------------------------------------------------------------------------}
data Teletype a = Get (Char -> Teletype a)
                | Put Char (Teletype a)
                | Return a

instance Functor Teletype where
  fmap f (Get t) = Get (fmap f . t)
  fmap f (Put c t)    = Put c (f <$> t)
  fmap f (Return x)   = Return (f x)

instance Applicative Teletype where
  pure = Return

  (Return f) <*> x          = f <$> x
  f          <*> (Return x) = ($ x) <$> f
  (Get f)    <*> t          = Get (\c -> f c <*> t)
  (Put c f)  <*> t          = Put c $ f <*> t

instance Monad Teletype where
  return = Return

  (Return x) >>= k = k x
  (Get f)    >>= k = Get (f >=> k) -- i.e. Get (\x -> f x >>= k)
  (Put c f)  >>= k = Put c (f >>= k)

-- | 'Foldable' and 'Traversable' are meaningless for a datatype involving
-- function types (i.e. Get <function>). Nonetheless, here are some definitions
-- that at least type-check.
instance Foldable Teletype where
  foldMap f (Return x) = f x
  foldMap _ (Get _)    = mempty
  foldMap f (Put _ x)  = foldMap f x

instance Traversable Teletype where
  traverse f (Return x)   = Return <$> f x
  traverse f (Get reader) = sequenceA $ Get (fmap f . reader)
  traverse f (Put c t)    = Put c <$> traverse f t

{-------------------------------------------------------------------------------
                            Maps and Keys
-------------------------------------------------------------------------------}

-- | Lookup a single key in the given 'Map'.
-- (re-implementation of 'Data.Map.lookup')
lookup :: Eq k
       => k         -- ^ key to lookup
       -> Map k v   -- ^ map from keys to values
       -> Maybe v   -- ^ 'Just' value if key occurs in map, 'Nothing' otherwise
lookup k m = case foldMapWithKey (\k' v -> [v | k == k']) m of
              [x] -> Just x
              _   -> Nothing

-- | Lookup several keys in the given 'Map'.
lookupAll :: Ord k
          => [k]       -- ^ keys to lookup
          -> Map k v   -- ^ map from keys to values
          -> Maybe [v] -- ^ 'Just' values if all keys occur in map, 'Nothing' otherwise
lookupAll ks m = traverse (`lookup` m) ks

-- | Lookup several keys in the given 'Map'.
lookupSome :: Ord k
           => [k]     -- ^ keys to lookup
           -> Map k v -- ^ map from keys to values
           -> [v]     -- ^ list of values for the keys that occur in map
lookupSome ks m = mapMaybe (`lookup` m) ks

{-------------------------------------------------------------------------------
                            Filter
-------------------------------------------------------------------------------}

-- | Filter elements of a 'Foldable'.
gfilter :: Foldable f
        => (a -> Bool) -- ^ predicate for filtering
        -> f a         -- ^ 'Foldable' container of elements
        -> [a]         -- ^ list of filtered elements
gfilter f = foldMap (\x -> [x | f x])
