module Main where

class Eq a => Entity a where
    getPosition :: a -> (Int, Int)
    getVelocity :: a -> (Int, Int)
    getAcceleration :: a -> (Int, Int)
    getColor :: a -> Int
    getShape :: a -> Int

data Paddle =
    Paddle {
        paddlePosX :: Int, paddlePosY :: Int,
        paddleVelX :: Int, paddleVelY :: Int,
        paddleAccX :: Int, paddleAccY :: Int,
        paddleColor :: Int,
        paddleHeight :: Int,
        playerNumber :: Int
    }
    deriving Eq

instance Entity Paddle where
    getPosition p = (paddlePosX p, paddlePosY p)
    getVelocity p = (paddleVelX p, paddleVelY p)
    getAcceleration p = (paddleAccX p, paddleAccY p)
    getColor = paddleColor
    getShape = paddleHeight

main = putStrLn "main"
