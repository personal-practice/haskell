module Main where

type I = Integer

main :: IO ()
main = do
    -- putStrLn $ show $ func1 2 [1, 2, 3]
    -- putStrLn $ show $ func2 (\x -> x > 2) (\x -> x +1) [1, 2, 3]
    -- putStrLn $ show $ func3 (+1) [1, 2, 3]

func1 :: I -> [I] -> [I]
-- func1 x l = map (\y -> y * x) l
-- func1 x = map (\y -> y * x)
func1 x = map (*x)

func2 :: (I -> Bool) -> (I -> I) -> [I] -> [I]
-- func2 f g l = filter f (map g l)
func2 f g = filter f . map g


func3 :: (I -> I) -> [I] -> [I]
-- func3 f l = l ++ map f l
-- func3 f l = ++ l (map f l)
-- func3 f = ++
-- func3 f =  (map f) . (++)

func4 l = map (\y -> y + 2)
                        (filter (\z -> z `elem` [1..10])
                                  (5:l))
