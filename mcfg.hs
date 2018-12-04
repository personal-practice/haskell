{-# LANGUAGE DataKinds
           , TypeOperators
           , ScopedTypeVariables
           , PolyKinds

           , IncoherentInstances
           , UndecidableInstances
           -- , AllowAmbiguousTypes

           , TypeFamilies
           , TypeInType
           , ExplicitForAll
#-}

{- TYPE FAMILIES -}
import GHC.TypeLits
import Data.Singletons.Prelude
import Data.Proxy


-- instance IsDyck '( x, y, z ) => IsDyck '( "a" ': x, "b" ': y, "c" ': z )
-- instance ( IsDyck '( x, y, z )
--          , IsDyck '( l, m, n )
--          , (xl ~ (x :++ l))
--          , (ym ~ (y :++ m))
--          , (zn ~ (z :++ n))
--          ) => IsDyck '( xl, ym, zn )


type family LAnd (a :: Bool) (b :: Bool) :: Bool where
  LAnd True True = True
  LAnd _    _    = True

type family LOr (a :: Bool) (b :: Bool) :: Bool where
  LOr True _    = True
  LOr _    True = True
  LOr _    _    = False

type family Equal (a :: k) (b :: k) :: Bool where
  Equal a a = True
  Equal a b = False

type WordTuple = ([Symbol], [Symbol], [Symbol])


type family AnyDyck (ws :: [WordTuple]) :: Bool where
  AnyDyck (x ': xs) = LOr (IsDyck x) (AnyDyck xs)
  AnyDyck '[]       = True

type family IsDyck (w :: WordTuple) :: Bool where
  IsDyck '( '["a"], '["b"], '["c"] ) = True
  IsDyck '( "a" ': "b" ': "c" ': xs, ys, zs ) = IsDyck '( xs, ys, zs )
  IsDyck '( "a" ': "b" ': xs, "c" ': ys, zs ) = IsDyck '( xs, ys, zs )
  IsDyck '( "a" ': "b" ': xs, ys, "c" ': zs ) = IsDyck '( xs, ys, zs )
  IsDyck '( "a" ': xs, "b" ': "c" ': ys, zs ) = IsDyck '( xs, ys, zs )
  IsDyck '( "a" ': xs, "b" ': ys, "c" ': zs ) = IsDyck '( xs, ys, zs )
  IsDyck '( "a" ': xs, ys, "b" ': "c" ': zs ) = IsDyck '( xs, ys, zs )
  IsDyck '( xs, "a" ': "b" ': "c" ': ys, zs ) = IsDyck '( xs, ys, zs )
  IsDyck '( xs, "a" ': "b" ': ys, "c" ': zs ) = IsDyck '( xs, ys, zs )
  IsDyck '( xs, "a" ': ys, "b" ': "c" ': zs ) = IsDyck '( xs, ys, zs )
  IsDyck '( xs, ys, "a" ': "b" ': "c" ': zs ) = IsDyck '( xs, ys, zs )


  -- IsDyck '( xs, ys, zs ) =
  --   LAnd (Equal xs ("a" ': xs'))
  --     (LAnd (Equal ys ("b" ': ys'))
  --        (LAnd (Equal zs ("c" ': zs'))
  --           (IsDyck '( xs', ys', zs' ))))
  IsDyck _ = False


type TestWord = '( '["a", "b", "c", "a"]
                 , '["b"]
                 , '["c"]
                 )


main :: IO ()
main = putStrLn recognize
  where
    recognize :: (IsDyck TestWord ~ True) => String
    recognize = "Look ma, it's a dyck!"


{- TYPE LITS (1)
import GHC.TypeLits
-- import Data.Singletons.Prelude
import Data.Proxy

class IsDyck a where
  recognize :: Proxy a -> IO ()
  recognize _ = print "Look ma, it's a dyck!"

instance IsDyck '( '[], '[], '[] )
instance IsDyck '( x, y, z ) => IsDyck '( "a" ': "b" ': "c" ': x, y, z )
instance IsDyck '( x, y, z ) => IsDyck '( "a" ': "b" ': x, "c" ': y, z )
instance IsDyck '( x, y, z ) => IsDyck '( "a" ': "b" ': x, y, "c" ': z )
instance IsDyck '( x, y, z ) => IsDyck '( "a" ': x, "b" ': "c" ': y, z )
instance IsDyck '( x, y, z ) => IsDyck '( "a" ': x, "b" ': y, "c" ': z )
instance IsDyck '( x, y, z ) => IsDyck '( "a" ': x, y, "b" ': "c" ': z )
instance IsDyck '( x, y, z ) => IsDyck '( x, "a" ': "b" ': "c" ': y, z )
instance IsDyck '( x, y, z ) => IsDyck '( x, "a" ': "b" ': y, "c" ': z )
instance IsDyck '( x, y, z ) => IsDyck '( x, "a" ': y, "b" ': "c" ': z )
instance IsDyck '( x, y, z ) => IsDyck '( x, y, "a" ': "b" ': "c" ': z )

-- instance IsDyck '( x, y, z ) => IsDyck '( "a" ': x, "b" ': y, "c" ': z )
-- instance ( IsDyck '( x, y, z )
--          , IsDyck '( l, m, n )
--          , (xl ~ (x :++ l))
--          , (ym ~ (y :++ m))
--          , (zn ~ (z :++ n))
--          ) => IsDyck '( xl, ym, zn )

type TestWord = '( '["a", "a", "a"]
                 , '["a", "a", "b", "b", "b", "b"]
                 , '["b", "c", "c", "c", "c", "c"]
                 )


main :: IO ()
main = recognize (undefined :: Proxy TestWord)
-}

{- TYPE LITS (2)

data Label (l :: Symbol) = Get

class Recognizable l where
  recognize :: Label l -> Bool
  recognize _ = True

instance Recognizable "abc" where

instance (Recognizable x, Recognizable y) => Recognizable () where

main = print $ recognize (Get :: Label "abc")
-}

{- -- REFIMENT TYPES
import Refined
import Data.Char

type Word = String

instance Predicate IsDyck Word where
  validate _ w
    | w == "abc" = Nothing
    | otherwise  = Just "Not a base dyck word"

type DyckWord = Refined IsDyck Word

main = print ($$(refineTH "abc") :: DyckWord)
-}


{- PURESCRIPT!

module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple

-- import Prim.Symbol as Sym
import Type.Data.Symbol


class Dyck (w1 :: Symbol) (w2 :: Symbol) (w3 :: Symbol)
instance base ::
  Dyck "a" "b" "c"
instance tripleInsertion ::
  ( Dyck x1 y1 z1
  , Dyck x2 y2 z2
  , AppendSymbol x1 x2 x
  , AppendSymbol y1 y2 y
  , AppendSymbol z1 z2 z
  ) => Dyck x y z

recognize :: forall w1 w2 w3. Dyck w1 w2 w3
                           => SProxy w1
                           -> SProxy w2
                           -> SProxy w3
                           -> String
recognize _ _ _ = "Look ma! It's a dyck!"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log (recognize (SProxy :: SProxy "aa")
                 (SProxy :: SProxy "bb")
                 (SProxy :: SProxy "cc"))

-}
