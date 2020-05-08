{-# Language FlexibleContexts  #-}
{-# Language OverloadedStrings  #-}
module Data.Geometry.Ipe.ParserPrimitives( runP, runP'
                                         , pMany, pMany1, pChoice
                                         , pChar, pSpace, pWhiteSpace, pInteger
                                         , pNatural, pPaddedNatural
                                         , (<*><>) , (<*><)
                                         , (<***>) , (<***) , (***>)
                                         , pMaybe , pCount , pSepBy
                                         , Parser, ParseError
                                         , pNotFollowedBy
                                         ) where


import           Text.Parsec(try)
import           Text.Parsec(ParsecT, Stream)
import           Text.Parsec.Text
import           Text.ParserCombinators.Parsec hiding (Parser,try)
import qualified Data.Text as T


runP'     :: Parser a -> T.Text -> (a, T.Text)
runP' p s = case runP p s of
             Left  e -> error $ show e
             Right x -> x

runP   :: Parser a -> T.Text -> Either ParseError (a,T.Text)
runP p = parse ((,) <$> p <*> getInput) ""


----------------------------------------------------------------------------
-- | reexporting some standard combinators

pMany :: Parser a -> Parser [a]
pMany = many

pMany1 :: Parser a -> Parser [a]
pMany1 = many1


pChoice :: [Parser a] -> Parser a
pChoice = choice . map try


pNatural :: Parser Integer
pNatural = read <$> pMany1 digit

-- | parses an integer with a prefix of zeros. Returns the total length of the
-- string parced (i.e. number of digits) and the resulting antural number.
pPaddedNatural :: Parser (Int, Integer)
pPaddedNatural = (\s -> (length s, read s)) <$> pMany1 digit

pInteger :: Parser Integer
pInteger = pNatural
           <|>
           negate <$> (pChar '-' *> pNatural)

pChar :: Char -> Parser Char
pChar = char

pSpace :: Parser Char
pSpace = pChar ' '

pWhiteSpace :: Parser [Char]
pWhiteSpace = pMany1 (space <|> newline)

pMaybe :: Parser a -> Parser (Maybe a)
pMaybe = optionMaybe

pCount :: Int -> Parser a -> Parser [a]
pCount = count

pSepBy :: Parser a -> Parser b -> Parser [a]
pSepBy = sepBy


-- | infix variant of notfollowed by
pNotFollowedBy :: Parser a -> Parser b -> Parser a
p `pNotFollowedBy` q = do { x <- p ; notFollowedBy' q ; return x }
    where
      -- | copy of the original notFollowedBy but replaced the error message
      -- to get rid of the Show dependency
      notFollowedBy' z     = try (do{ _ <- try z; unexpected "not followed by" }
                                    <|> return ()
                                 )

----------------------------------------------------------------------------
-- | Running parsers in reverse

infix 1 <*><>, <*><

-- | Runs parser q ``in reverse'' on the end of the input stream
(<*><>) ::  (Reversable s, Stream s m t)
        => ParsecT s u m (a -> b) -> ParsecT s u m a -> ParsecT s u m b
p <*><> q = do
  rev
  x <- q
  rev
  f <- p
  return $ f x


(<*><)   :: (Stream s m t, Reversable s)
          => ParsecT s u m b -> ParsecT s u m a -> ParsecT s u m b
p <*>< q = const <$> p <*><> q


rev :: (Reversable s, Stream s m t) => ParsecT s u m ()
rev = getInput >>= (setInput . reverseS)

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
(<***>) :: Monad m => m (t -> b) -> m t -> m b
p <***> q = do
  x <- q
  f <- p
  return $ f x

-- | the variants with missing brackets
(***>) :: Monad m => m a -> m b -> m b
p ***> q = (\_ s -> s) <$> p <***> q

(<***) :: Monad m => m b -> m t -> m b
p <*** q = (\s _ -> s) <$> p <***> q

class Reversable s where
  reverseS :: s -> s

instance Reversable [c] where
  reverseS = reverse

instance Reversable T.Text where
  reverseS = T.reverse
