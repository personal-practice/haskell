module Main where

import Control.Monad

class MonadTrans t where
    lift :: Monad m => m a -> t m a

newtype State state a =
               State (state -> (state, a))
-- Wrap state monad around another arbitrary monad
newtype StateT state m a =
               StateT (state -> m (state, a))

-- Define transformer
instance MonadTrans (StateT state) where
    lift m = StateT (\s -> do
        a <- m
        return (s, a))

-- Define StateT monad
instance Monad m => Monad (StateT state m) where
    return a = StateT (\s -> return (s, a))
    StateT m >>= k = StateT (\s -> do
        (s', a) <- m s
        let StateT m' = k a
        m' s')
    fail s = StateT (\_ -> fail s)

-- Define monad utilities, analogous to getState, putState, runStateM
getT :: Monad m => StateT s m s
getT = StateT (\s -> return (s, s))

putT :: Monad m => s -> StateT s m ()
putT s = StateT (\_ -> return (s, ()))

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT (StateT m) state = do
    (s’, a) <- m state
    return a

-- mapTree with monad transformers
mapTreeM :: (MonadTrans t, Monad (t IO), Show a) =>
                      (a -> t IO a1) -> Tree a -> t IO (Tree a1)
mapTreeM action (Leaf a) = do
    lift (putStrLn ("Leaf " ++ show a))
    b <- action a
    return (Leaf b)
mapTreeM action (Branch lhs rhs) = do
    lift (putStrLn "Branch")
    lhs' <- mapTreeM action lhs
    rhs' <- mapTreeM action rhs
    return (Branch lhs' rhs')

numberTree tree = mapTreeM number tree
    where number v = do
            cur <- getT
            putT (cur + 1)
            return (v, cur)
    -- e.g. evalStateT (numberTree testTree) 0

-- searchAll that allows cycles
searchAll5 g@(Graph vl el) src dst
    | src == dst = do
            visited <- getT
            putT (src:visited)
            return [src]

    | otherwise = do
            visited <- getT
            putT (src:visited)
            if src `elem` visited
                    then mzero
                    else search' el
    where
        search' [] = mzero
        serach' ((u,v,_):es)
            | src == u =
                (do path <- searchAll5 g v dst)
                    return (u:path)) `mplus`
                search' es
            | otherwise = search' es
