{-# LANGUAGE MultiParamTypeClasses #-}
module Teletype where

import           Control.Monad                ((>=>))
import           Control.Monad.State.Lazy
import qualified Control.Monad.Trans.RWS.Lazy as RWS
import           Prelude                      hiding (getChar, putChar)
import qualified Prelude                      as Pre (getChar, putChar)

data Teletype a = Get (Char -> Teletype a)
                | Put Char (Teletype a)
                | Return a

echo :: Teletype a
echo = Get $ \c -> Put c echo

-------------------------------- Question 1 ------------------------------------
getLine :: Teletype String
getLine = go ""
  where go s = Get $ \c ->
          if c == '\n' then
            Return s
          else
            go (s ++ [c])

-------------------------------- Question 2 ------------------------------------
instance Functor Teletype where
  fmap f (Get t)    = Get (fmap f . t)
  fmap f (Put c t)  = Put c (f <$> t)
  fmap f (Return x) = Return (f x)

instance Applicative Teletype where
  pure = Return
  (Return f) <*> x          = f <$> x
  (Get f)    <*> t          = Get (\c -> f c <*> t)
  (Put c f)  <*> t          = Put c (f <*> t)

instance Monad Teletype where
  return = Return
  (Return x) >>= k = k x
  (Get f)    >>= k = Get (f >=> k)
  (Put c f)  >>= k = Put c (f >>= k)

-------------------------------- Question 3 ------------------------------------
getChar :: Teletype Char
getChar = Get Return

putChar :: Char -> Teletype ()
putChar c = Put c $ Return ()

-------------------------------- Question 4 ------------------------------------
instance MonadState Char Teletype where
  get = getChar
  put = putChar

-- Difference with 'State'
-- ~~~~~~~~~~~~~~~~~~~~~~~

-- First of all, using 'State' would not allow us to reuse out 'Teletype' monad,
-- hence we would have to use the 'StateT' monad transformer, i.e.
-- > type TeletypeWithState = StateT s Teletype

-- Moreover, assuming a stack of multiple monad transformers, we would have
-- to 'lift' our way throuugh the appropriate stack level of 'StateT', in order
-- to use 'get'/'put'/'state'. By using the 'MonadState' interface, the 'mtl'
-- library does this for us, e.g.
--
-- > instance MonadState s  => MonadState s (ListT m) where
-- >   get = lift get
-- >   put = lift . put
-- >   state = lift . state

-- In terms of behavioiurr, the 'MonadState' interface allows us to re-define
-- the semantics of 'get'/'put' according to our needs. The interface defined
-- above is behaving differently than 'State', since it does not actually update
-- a state of a single 'Char' by using 'put', but rather gives a "real-world"
-- semantics, where 'get' corresponds to waiting for the user to input a
-- character and 'put' to write a character.
--   Hence, a value you 'get' immediately after you 'put' another value, will not
-- guarantee the two values are the same. This would however be the case with
-- 'StateT', which would merely model a state of a single character.

-------------------------------- Question 5 ------------------------------------
runConsole :: Teletype a -> IO a
runConsole (Return x) = return x
runConsole (Get k)    = Pre.getChar >>= runConsole . k
runConsole (Put c k)  = Pre.putChar c >> runConsole k

-------------------------------- Question 6 ------------------------------------
type TeletypeRW = RWS.RWS [Char] () [Char]

runRWS :: Teletype a -> TeletypeRW a
runRWS (Return x) = return x
runRWS (Get k) = do
  input <- RWS.ask
  RWS.local tail $ runRWS (k $ head input)
runRWS (Put c k) = do
  RWS.modify (++ [c])
  runRWS k

mockConsole :: Teletype a -> [Char] -> (a, [Char])
mockConsole tele input = (a, s)
  where (a, s, _) = RWS.runRWS (runRWS tele) input []
