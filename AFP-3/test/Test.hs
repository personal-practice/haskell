{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad.State.Class
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import qualified Fix                       (foldr, y)
import           Nested
import           Teletype

import           Prelude                   hiding (getLine)

main :: IO ()
main =
  defaultMain $ testGroup "Tests"
    [ testGroup "Fix"
        [ testProperty "Fix.foldr equivalent to Prelude.foldr" $ \(l :: [Int]) ->
            let impl fold = fold (\i s -> show i ++ s) "" l
            in  impl foldr == impl Fix.foldr
        , testCase "y combinator works" $
            let fac :: Integer -> Integer
                fac = Fix.y $ \fac' n ->
                        if n == 0 then
                          1
                        else
                          n * fac' (n - 1)
            in  fac 11 @?= 39916800
        ]
    , testGroup "TeletypeIO"
        [ testCase "mock (monad)" $
            let addition :: Teletype Int
                addition = do n <- getLine
                              n' <- getLine
                              let res = read n + read n' :: Int
                              mapM_ put (show res)
                              return res
                input = "111\n111\n11"
            in  mockConsole addition input @?= (222, "222")
        , testCase "mock (applicative)" $
            let addition :: Teletype Int
                addition = (+) <$> (read <$> getLine) <*> (read <$> getLine)
                input = "111\n111\n11"
            in  mockConsole addition input @?= (222, [])
        ]
    , testGroup "Square"
        [ testCase "equality" $
            let n = 666 :: Int
                sq1 = Succ $ Succ $ Zero $ Cons r10 $ Cons r01 Nil
                        where r10 = Cons n $ Cons (n+1) Nil
                              r01 = Cons (n+1) $ Cons n Nil
                sq2 = Succ $ Succ $ Zero $ Cons r10 $ Cons r01 Nil
                        where r10 = Cons (n-1+1) $ Cons (n-1+2) Nil
                              r01 = Cons (n-1+2) $ Cons (n-1+1) Nil
            in  sq1 @?= sq2
        , testCase "mapping" $
        let n = 666 :: Int
            sq1 = Succ $ Succ $ Zero $ Cons r10 $ Cons r01 Nil
                    where r10 = Cons n $ Cons (n+1) Nil
                          r01 = Cons (n+1) $ Cons n Nil
            sq2 = Succ $ Succ $ Zero $ Cons r10 $ Cons r01 Nil
                    where r10 = Cons (n+1) $ Cons (n+2) Nil
                          r01 = Cons (n+2) $ Cons (n+1) Nil
        in  (+ 1) <$> sq1 @?= sq2
        ]
    ]

instance (Show a) => Show (Square' t a) where
  show (Zero _)  = "Zero"
  show (Succ xs) = "Succ " ++ show xs
