module Main where

pt :: Int -> Float
pt x = 0.01 * fromIntegral x

ptp, ptp' :: [Int] -> [Float]
ptp xs = map (\x -> 0.01 * fromIntegral x) xs
ptp' xs = [0.01 * fromIntegral x | x <- xs]

main :: IO ()
main = do
  let input  = 120
      weird  = 1.1999999
      normal = 1.2
  print [ ptp [input] == [weird]
        , ptp' [input] == [weird]
        , (0.01 * fromIntegral input) == normal
        , pt input == weird
        ]
