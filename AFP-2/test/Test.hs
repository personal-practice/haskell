{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
import Control.Monad          ((>=>))
import Control.Monad.Identity
import Data.Foldable
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Proxy
import GHC.Generics
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck

import Datatypes

main :: IO ()
main = defaultMain $ testGroup "Tests"
         [ genLaws "Tree" 3 (Proxy :: Proxy Tree)
         , genLaws "RoseTree" 3 (Proxy :: Proxy RoseTree)
         -- , genLaws "Teletype" 2 (Proxy :: Proxy Teletype)
         ]
  where genLaws t d f = laws LawsConfig { typeName = t
                                        , depth = d
                                        , proxyF = f
                                        , proxyA = Proxy :: Proxy Bool
                                        , proxyB = Proxy :: Proxy (Maybe Bool)
                                        , proxyC = Proxy :: Proxy Bool
                                        , proxyM = Proxy :: Proxy String
                                        }

data LawsConfig (f :: * -> *) a b c m = LawsConfig
  { typeName :: String
  , depth    :: Int
  , proxyF   :: Proxy f
  , proxyA   :: Proxy a
  , proxyB   :: Proxy b
  , proxyC   :: Proxy c
  , proxyM   :: Proxy m
  }

laws :: forall f a b c m.
  ( Show a, Show b, Show c, Show m, Show (f a), Show (f b), Show (f c), Show (f m)
  , Show (f (a -> b)), Show (f (b -> c))
  , Eq (f a), Eq (f b), Eq (f c), Eq m, Eq (f Char), Eq1 f
  , Serial IO a, Serial IO b, Serial IO m, Serial Identity a, Serial Identity b
  , Serial IO (f a), Serial IO (f b), Serial IO (f c), Serial IO (f m)
  , CoSerial IO a, CoSerial IO b, Serial IO (f (a -> b)), Serial IO (f (b -> c))
  , Functor f, Applicative f, Monad f, Foldable f, Traversable f, Monoid m
  ) => LawsConfig f a b c m -> TestTree
laws (LawsConfig name d _ _ _ _ _) =
  localOption (SmallCheckDepth d) $ testGroup name
    [ testGroup "FunctorLaws"
        [ testProperty "identity" (functorId :: f a -> Bool)
        , testProperty "composition" (functorComp :: f a -> (a -> b) -> (b -> a) -> Bool)
        ]
    , testGroup "ApplicativeLaws"
        [ testProperty "identity" (appId :: f a -> Bool)
        , testProperty "composition" (appComp :: f a -> f (a -> b) -> f (b -> c) -> Bool)
        , testProperty "homomorphism" (appHomo (Proxy :: Proxy f) :: (a -> a) -> a -> Bool)
        , testProperty "interchange" (appInter :: f (a -> b) -> a -> Bool)
        ]
    , testGroup "MonadLaws"
        [ testProperty "left identity" (monadIdL :: a -> (a -> f b) -> Bool)
        , testProperty "right identity" (monadIdR :: f a -> Bool)
        , testProperty "associativity" (monadAssoc :: f a -> (a -> f b) -> (b -> f c) -> Bool)
        , testProperty "with functor" (monadFunctor :: (a -> b) -> f a -> Bool)
        , testProperty "with applicative" (monadApp :: f (a -> b) -> f a -> Bool)
        ]
    , testGroup "FoldableLaws"
        [ testProperty "identity" (foldableId :: f m -> Bool)
        , testProperty "with functor" (foldableFunctor :: f a -> (a -> m) -> Bool)
        ]
    , testGroup "TraversableLaws"
        [ testProperty "identity" (traversableId :: f a -> Bool)
        , testProperty "identity2" (traversableId2 :: f a -> Bool)
        , testProperty "composition" (traversableComp :: (a -> f b) -> (b -> String) -> f a -> Bool)
        -- , testProperty "naturality" (traversableNat :: (forall x. Maybe x -> Maybe x) -> (a -> Maybe a) -> f a -> Bool)
        -- *** GHC doesn't yet support impredicative polymorphism :(
        ]
    ]

{-------------------------------------------------------------------------------
                                Laws
-------------------------------------------------------------------------------}
functorId :: (Functor f, Eq (f a))
          => f a -> Bool
functorId x = (id <$> x) == x

functorComp :: (Functor f, Eq (f c))
            => f a -> (a -> b) -> (b -> c) -> Bool
functorComp x g f = fmap (f . g) x == (fmap f . fmap g) x

appId :: (Applicative f, Eq (f a))
      => f a -> Bool
appId u = (pure id <*> u) == u

appComp :: (Applicative f, Eq (f c))
        => f a -> f (a -> b) -> f (b -> c) -> Bool
appComp w v u = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

appHomo :: forall f a. (Applicative f, Eq (f a))
        => Proxy f -> (a -> a) -> a -> Bool
appHomo _ f x = (pure f <*> pure x :: f a) == (pure (f x) :: f a)

appInter :: (Applicative f, Eq (f b))
         => f (a -> b) -> a -> Bool
appInter u x = (u <*> pure x) == (pure ($ x) <*> u)

monadIdL :: (Monad m, Eq (m b))
         => a -> (a -> m b) -> Bool
monadIdL a f = (return a >>= f) == f a

monadIdR :: (Monad m, Eq (m a))
         => m a -> Bool
monadIdR m = (m >>= return) == m

monadAssoc :: (Monad m, Eq (m c))
           => m a -> (a -> m b) -> (b -> m c) -> Bool
monadAssoc m f g = ((m >>= f) >>= g) == (m >>= (f >=> g))

monadApp :: (Monad m, Eq (m b)) => m (a -> b) -> m a -> Bool
monadApp m1 m2  = (m1 <*> m2) == do { f <- m1; x <- m2; return (f x) }

monadFunctor :: (Monad m, Eq (m b))
             => (a -> b) -> m a -> Bool
monadFunctor f xs = (f <$> xs) == (xs >>= return . f)

foldableFunctor :: (Foldable t, Functor t, Monoid m, Eq m)
                => t a -> (a -> m) -> Bool
foldableFunctor t f = foldMap f t == (fold . fmap f) t

foldableId :: (Foldable t, Monoid a, Eq a)
           => t a -> Bool
foldableId m = fold m == foldMap id m

traversableNat :: (Traversable t, Applicative f, Applicative g, Eq (g (t b)))
               => (forall x. f x -> g x) -> (a -> f b) -> t a -> Bool
traversableNat t f m = (t . traverse f) m == traverse (t . f) m

traversableId :: (Traversable t, Eq (t a))
              => t a -> Bool
traversableId m = traverse Identity m == Identity m

traversableId2 :: (Traversable t, Eq (t a))
               => t a -> Bool
traversableId2 m = traverse Identity m == Identity m

traversableComp :: (Traversable t, Applicative f, Applicative g, Eq1 f, Eq1 g, Eq (t c))
                => (a -> f b) -> (b -> g c) -> t a -> Bool
traversableComp f g m =
  traverse (Compose . fmap g . f) m == (Compose . fmap (traverse g) . traverse f) m

{-------------------------------------------------------------------------------
                            Extra Instances
-------------------------------------------------------------------------------}
deriving instance Eq a => Eq (Tree a)
deriving instance Eq a => Eq (RoseTree a)
instance Eq a => Eq (Teletype a) where
  (Return x) == (Return x') = x == x'
  (Put c t) == (Put c' t') = c == c' && t == t'
  (Get _) == (Get _) = True
  _ == _ = False

deriving instance Show a => Show (Tree a)
deriving instance Show a => Show (RoseTree a)
instance Show a => Show (Teletype a) where
  show (Return x) = "Return " ++ show x
  show (Get _)    = "Get λ"
  show (Put c t)  = "Put " ++ [c] ++ " " ++ show t

deriving instance Generic (Tree a)
deriving instance Generic (RoseTree a)
deriving instance Generic (Teletype a)

deriving instance Serial m a => Serial m (Tree a)
deriving instance Serial m a => Serial m (RoseTree a)
deriving instance Serial m a => Serial m (Teletype a)

instance Eq1 Tree where
  liftEq eq (Leaf a) (Leaf b)       = a `eq` b
  liftEq eq (Node l r) (Node l' r') = liftEq eq l l' && liftEq eq r r'
  liftEq _ _ _                      = False
instance Eq1 RoseTree where
  liftEq _ RoseLeaf RoseLeaf = True
  liftEq eq (RoseNode x rs) (RoseNode x' rs') =
       x `eq` x'
    && length rs == length rs'
    && and [liftEq eq r r' | r <- rs | r' <- rs']
  liftEq _ _ _ = False
instance Eq1 Teletype where
  liftEq eq (Return x) (Return x') = x `eq` x'
  liftEq eq (Put c t) (Put c' t')  = c == c' && liftEq eq t t'
  liftEq _ (Get _) (Get _)         = True
  liftEq _ _ _                     = False
