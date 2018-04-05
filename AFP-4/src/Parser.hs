{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Parser where

import Control.Applicative
import Control.Arrow              (first)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Functor.Identity

newtype ErrorMsg = ErrorMsg String deriving (Eq, Show)

newtype Parser a = Parser { parse :: String -> Either ErrorMsg (a, String) }

instance Functor Parser where
  fmap f (Parser parser) = Parser ((first f <$>) . parser)

instance Applicative Parser where
  pure x = Parser $ \s -> Right (x, s)

  Parser pf <*> px = Parser $ \s ->
    case pf s of
      Left err      -> Left err
      Right (f, s') -> parse (f <$> px) s'

instance Monad Parser where
  return = pure

  Parser p >>= k = Parser $ \s ->
    case p s of
      Left err      -> Left err
      Right (x, s') -> parse (k x) s'

instance Alternative Parser where
  empty = Parser $ const $ Left $ ErrorMsg "empty"

  Parser p <|> Parser p' = Parser $ \s ->
    case p s of
      Left _ -> p' s
      res    -> res

-- | Transformers.
type ParserT = StateT String (Except ErrorMsg)

instance {-# OVERLAPS #-} Alternative ParserT where
  empty = (StateT . const . except . Left . ErrorMsg) "empty"

  p <|> p' = StateT $ \s ->
    case runParserT p s of
      Left _  -> runStateT p' s
      Right r -> return r

runParserT :: ParserT a -> String -> Either ErrorMsg (a, String)
runParserT p s = runIdentity $ runExceptT $ runStateT p s
