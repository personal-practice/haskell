{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module GenParser where

import Control.Applicative ((<|>))
import GHC.Generics

import Parser
import ParserUtils

-- | Concrete typeclass.
class Parse a where
  -- | Returns the parser corresponding to this datatype.
  parser :: ParserT a
  parser = parser' defConfig

  -- | Run parser.
  runP :: String -> Either ErrorMsg (a, String)
  runP = runParserT parser

  -- | Returns the parser corresponding to this datatype (under a configuration).
  parser' :: Config -> ParserT a

  -- | Default implementation derived from the 'Generic' representation.
  default parser' :: (Generic a, GParse (Rep a)) => Config -> ParserT a
  parser' cg = to <$> gparser cg (from (undefined :: a))

-- | Helpful information for parsing.
data Config = Config
  { constructor :: (String, Fixity)
  , isRecord    :: Bool
  , nestLevel   :: Int
  , inPair      :: Bool
  }
defConfig :: Config
defConfig = Config ("", Prefix) False 0 False

-- | Generic typeclass.
class GParse f where
  gparser :: Config -> f a -> ParserT (f a)

-- | Metadata: Datatype.
instance (Datatype d, GParse f) => GParse (M1 D d f) where
  gparser config _ = M1 <$> gparser config (undefined :: f a)

-- | Metadata: Constructor.
instance (Constructor c, GParse f) => GParse (M1 C c f) where
  gparser cg c = M1 <$> inner
    where
      inner = if conIsRecord c then simple <|> record else simple
      config = Config (conName c, conFixity c) False (nestLevel cg + 1) (inPair cg)
      simple = gparser config (undefined :: f a)
      record = conName c ~>
                 braces (gparser (config {isRecord = True}) (undefined :: f a))

-- | Metadata: Record selector.
instance (Selector s, GParse f) => GParse (M1 S s f) where
  gparser config s = M1 <$>
    if isRecord config then
      selName s ~> "=" ~>
      gparser (config {nestLevel = 0}) (undefined :: f a)
    else
      gparser config (undefined :: f a)

-- | Sums.
instance (GParse f, GParse g) => GParse (f :+: g) where
  gparser config _ =
       (L1 <$> gparser config (undefined :: f a))
   <|> (R1 <$> gparser config (undefined :: g b))

-- | Products.
instance (GParse f, GParse g) => GParse (f :*: g) where
  gparser cg _ = do
    let sep = if isRecord cg then "," else ""
    let putParens = if nestLevel cg > 1 then parens else optParens
    let fixity = snd $ constructor cg
    let infixC = case fixity of
                  Infix _ _ -> True
                  _         -> False
    let begin = if infixC then "" else fst (constructor cg)
    if inPair cg || (infixC && nestLevel cg < 2) || isRecord cg then
      optParens $ do
        a <- gparser cg (undefined :: f a) <~ sep
        b <- gparser (cg {constructor = ("", Prefix)}) (undefined :: g a)
        return $ a :*: b
    else
      putParens $ begin ~> do
        a <- gparser (cg {inPair = True}) (undefined :: f a) <~ sep
        b <- gparser (cg {inPair = True, constructor = ("", fixity)}) (undefined :: g a)
        return $ a :*: b

-- | Constants.
instance (Parse a) => GParse (K1 i a) where
  gparser (Config (con, fixity) record nest pair) _ = K1 <$>
    if pair then noParens
    else case fixity of
      Prefix    -> putParens $ (if record then "" else con) ~> recP
      Infix _ _ -> noParens
    where
      noParens = optParens $ recP <~ end
      putParens = if nest > 1 then parens else optParens
      end = case fixity of
              Prefix -> ""
              Infix _ _ -> con
      recP = parser' defConfig {nestLevel = nest}

-- | Nullary constructor.
instance GParse U1 where
  gparser cg _ | isRecord cg = return U1
               | otherwise   = fst (constructor cg) ~> return U1

-- | Void constructor.
instance GParse V1 where
  gparser = undefined

----------------------------- Base Instances -----------------------------------

instance Parse Bool where
  parser = (string "True" >> return True)
       <|> (string "False" >> return False)

instance Parse Char where
  parser' _ = anyChar

instance Parse Integer where
  parser' _ = read <$>
    ((++) <$> (char '-' ??)
          <*> many (oneOf $ map (head . show @Integer) [0..9]))

instance Parse Int where
  parser' _ = fromInteger <$> parser

instance {-# OVERLAPS #-} Parse String where
  parser' _ = quotes ((parser `without` "\"")^*)

instance {-# OVERLAPPABLE #-} Parse a => Parse [a] where
  parser' _ = list parser

-- Limitation
-- ~~~~~~~~~~
-- Associativity/Precedence of infix constructors is not supported.
