{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
module Data.Geometry.Ipe.PathParser where

import           Numeric

import           Control.Applicative
import           Control.Monad

import           Data.Bifunctor
import           Data.Monoid(mconcat)
import           Data.Semigroup
import           Data.Validation(AccValidation(..))

import           Data.Char(isSpace)
import           Data.Ratio

import           Text.Parsec.Error(messageString, errorMessages)
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.Geometry.Transformation
import           Data.Geometry.Ipe.ParserPrimitives
import           Data.Geometry.Ipe.Types(Operation(..))
import           Data.Text(Text)
import qualified Data.Text as T

-- type Matrix d m r = ()


-----------------------------------------------------------------------
-- | Represent stuff that can be used as a coordinate in ipe. (similar to show/read)

class Num r => Coordinate r where
    fromSeq :: Integer -> Maybe Integer -> r

defaultFromSeq :: (Ord r, Fractional r) => Integer -> Maybe Integer -> r
defaultFromSeq x Nothing  = fromInteger x
defaultFromSeq x (Just y) = let x'        = fromInteger x
                                y'        = fromInteger y
                                asDecimal = head . dropWhile (>= 1) . iterate (* 0.1)
                            in signum x' * (abs x' + asDecimal y')

instance Coordinate Double where
  fromSeq = defaultFromSeq

instance Coordinate (Ratio Integer) where
    fromSeq x  Nothing = fromInteger x
    fromSeq x (Just y) = fst . head $ readSigned readFloat (show x ++ "." ++ show y)

-----------------------------------------------------------------------
-- | Running the parsers

readCoordinate :: Coordinate r => Text -> Either Text r
readCoordinate = runParser pCoordinate

readPoint :: Coordinate r => Text -> Either Text (Point 2 r)
readPoint = runParser pPoint

runParser   :: Parser a -> Text -> Either Text a
runParser p = bimap errorText fst . runP p

-- Collect errors
data Either' l r = Left' l | Right' r deriving (Show,Eq)
instance (Semigroup l, Semigroup r, Monoid r) => Monoid (Either' l r) where
  mempty = Right' mempty
  (Left' l)  `mappend` (Left' l')  = Left' $ l <> l'
  (Left' l)  `mappend` _           = Left' l
  _          `mappend` (Left' l')  = Left' l'
  (Right' r) `mappend` (Right' r') = Right' $ r <> r'

either' :: (l -> a) -> (r -> a) -> Either' l r -> a
either' lf _  (Left' l)  = lf l
either' _  rf (Right' r) = rf r
-- TODO: Use Validation instead of this home-brew one

readPathOperations :: Coordinate r => Text -> Either Text [Operation r]
readPathOperations = unWrap . mconcat . map (wrap . runP pOperation)
                   . clean . splitKeepDelims "mlcqeasuh"
    where
      -- Unwrap the Either'. If it is a Left containing all our errors,
      -- combine them into one error. Otherwise just ReWrap it in an proper Either
      unWrap = either' (Left . combineErrors) Right
      -- for the lefts: wrap the error in a list, for the rights: we only care
      -- about the result, so wrap that in a list as well. Collecting the
      -- results is done using the Semigroup instance of Either'
      wrap   = either (Left' . (:[])) (Right' . (:[]) . fst)
      -- Split the input string in pieces, each piece represents one operation
      trim   = T.dropWhile isSpace
      clean  = filter (not . T.null) . map trim
      -- TODO: Do the splitting on the Text rather than unpacking and packing
      -- the thing

errorText :: ParseError -> Text
errorText = T.pack . unlines . map messageString . errorMessages

combineErrors :: [ParseError] -> Text
combineErrors = T.unlines . map errorText


splitKeepDelims          :: [Char] -> Text -> [Text]
splitKeepDelims delims t = maybe mPref continue $ T.uncons rest
  where
    mPref           = if T.null pref then [] else [pref]
    (pref,rest)     = T.break (`elem` delims) t
    continue (c,t') = pref `T.snoc` c : splitKeepDelims delims t'


readMatrix :: Coordinate r => Text -> Either Text (Matrix 3 3 r)
readMatrix = bimap errorText fst . runP pMatrix

-----------------------------------------------------------------------
-- | The parsers themselves


pOperation :: Coordinate r => Parser (Operation r)
pOperation = pChoice [ MoveTo       <$> pPoint                         *>> 'm'
                     , LineTo       <$> pPoint                         *>> 'l'
                     , CurveTo      <$> pPoint <*> pPoint' <*> pPoint' *>> 'c'
                     , QCurveTo     <$> pPoint <*> pPoint'             *>> 'q'
                     , Ellipse      <$> pMatrix                        *>> 'e'
                     , ArcTo        <$> pMatrix <*> pPoint'            *>> 'a'
                     , Spline       <$> pPoint `pSepBy` pWhiteSpace    *>> 's'
                     , ClosedSpline <$> pPoint `pSepBy` pWhiteSpace    *>> 'u'
                     , pChar 'h'  *> pure ClosePath
                     ]
             where
               pPoint' = pWhiteSpace *> pPoint
               p *>> c = p <*>< pWhiteSpace ***> pChar c


pPoint :: Coordinate r => Parser (Point 2 r)
pPoint = point2 <$> pCoordinate <* pWhiteSpace <*> pCoordinate


pCoordinate :: Coordinate r => Parser r
pCoordinate = fromSeq <$> pInteger <*> pDecimal
              where
                pDecimal = pMaybe (pChar '.' *> pInteger)

pMatrix :: Coordinate r => Parser (Matrix 3 3 r)
pMatrix = (\a b -> mkMatrix (a:b)) <$> pCoordinate
                                   <*> pCount 5 (pWhiteSpace *> pCoordinate)


-- | Generate a matrix from a list of 6 coordinates.
mkMatrix               :: Coordinate r => [r] -> Matrix 3 3 r
mkMatrix [a,b,c,d,e,f] = Matrix $ v3 (v3 a c e)
                                     (v3 b d f)
                                     (v3 0 0 1)
                           -- We need the matrix in the following order:
                         -- 012
                         -- 345
                         --
                         -- But ipe uses the following order:
                         -- 024
                         -- 135
mkMatrix _             = error "mkMatrix: need exactly 6 arguments"
