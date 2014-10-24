{-# Language FlexibleContexts
  #-}
module Data.Geometry.Ipe.ParserPrimitives( runP, runP'
                                         , pMany, pMany1, pChoice
                                         , pChar, pSpace, pWhiteSpace, pInteger, pNatural
                                         , (<*><>) , (<*><)
                                         , (<***>) , (<***) , (***>)
                                         , pMaybe , pCount , pSepBy
                                         , Parser(..) , ParseError
                                         , pNotFollowedBy
                                         ) where


-- import Data.Functor.Identity

import Control.Applicative hiding (many,(<|>))

import Text.Parsec(ParsecT(..),Parsec, Stream(..))
import Text.ParserCombinators.Parsec

runP p s = case runP' p s of
             Left  e -> error $ show e
             Right x -> x

runP' p = parse ((,) <$> p <*> getInput) ""


----------------------------------------------------------------------------
-- | reexporting some standard combinators

pMany = many
pMany1 = many1

pChoice = choice . map try


pNatural :: Parser Integer
pNatural = read <$> pMany1 digit

pInteger :: Parser Integer
pInteger = pNatural
           <|>
           negate <$> (pChar '-' *> pNatural)

pChar :: Char -> Parser Char
pChar = char

pSpace :: Parser Char
pSpace = pChar ' '

pWhiteSpace :: Parser Char
pWhiteSpace = space

pMaybe :: Parser a -> Parser (Maybe a)
pMaybe = optionMaybe

pCount :: Int -> Parser a -> Parser [a]
pCount = count

pSepBy :: Parser a -> Parser b -> Parser [a]
pSepBy = sepBy


-- | infix variant of notfollowed by
p `pNotFollowedBy` q = do { x <- p ; notFollowedBy' q ; return x }
    where
      -- | copy of the original notFollowedBy but replaced the error message
      -- to get rid of the Show dependency
      notFollowedBy' p     = try (do{ c <- try p; unexpected "not followed by" }
                                    <|> return ()
                                 )

----------------------------------------------------------------------------
-- | Running parsers in reverse

infix 1 <*><>, <*><

-- | Runs parser q ``in reverse'' on the end of the input stream
(<*><>) ::  Stream [c] m t => ParsecT [c] u m (a -> b) -> ParsecT [c] u m a -> ParsecT [c] u m b
p <*><> q = do
  rev
  x <- q
  rev
  f <- p
  return $ f x


p <*>< q = (\a _ -> a) <$> p <*><> q


rev :: Stream [c] m t => ParsecT [c] u m ()
rev = getInput >>= (setInput . reverse)

-- as :: Parser String
-- as = many (char 'a')

-- foo :: Parser String
-- foo = reverse <$> (string . reverse $ "foo")

-- prs :: Parser (String,String)
-- prs = (,) <$> as <*>< foo'

-- foo' :: Parser String
-- foo' = spaces ***> foo

-- (<***>) :: Parser (a -> b)

infixr 2 <***>, ***>, <***

-- | run the parsers in reverse order, first q, then p
p <***> q = do
  x <- q
  f <- p
  return $ f x

-- | the variants with missing brackets
p ***> q = (\_ s -> s) <$> p <***> q

p <*** q = (\s _ -> s) <$> p <***> q
