{-# Language OverloadedStrings #-}
{-# Language DefaultSignatures #-}
module Data.Geometry.Ipe.PathParser where

import           Data.Bifunctor
import           Data.Char (isSpace)
import           Data.Ext (ext)
import           Data.Geometry.Box
import           Data.Geometry.Ipe.ParserPrimitives
import           Data.Geometry.Ipe.Path (Operation(..))
import           Data.Geometry.Point
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import           Data.Ratio
import           Data.RealNumber.Rational
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parsec.Error (messageString, errorMessages)


-----------------------------------------------------------------------
-- | Represent stuff that can be used as a coordinate in ipe. (similar to show/read)

class Fractional r => Coordinate r where
    -- reads a coordinate. The input is an integer representing the
    -- part before the decimal point, and a length and an integer
    -- representing the part after the decimal point
    fromSeq :: Integer -> Maybe (Int, Integer) -> r
    default fromSeq :: (Ord r, Fractional r) => Integer -> Maybe (Int, Integer) -> r
    fromSeq = defaultFromSeq

defaultFromSeq                :: (Ord r, Fractional r)
                              => Integer -> Maybe (Int, Integer) -> r
defaultFromSeq x Nothing      = fromInteger x
defaultFromSeq x (Just (l,y)) = let x'          = fromInteger x
                                    y'          = fromInteger y
                                    asDecimal a =  a * (0.1 ^ l)
                                    z           = if x' < 0 then (-1) else 1
                                in z * (abs x' + asDecimal y')

instance Coordinate Double
instance Coordinate Float
instance Coordinate (Ratio Integer)
instance Coordinate (RealNumber p)

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

instance (Semigroup l, Semigroup r) => Semigroup (Either' l r) where
  (Left' l)  <> (Left' l')  = Left' $ l <> l'
  (Left' l)  <> _           = Left' l
  _          <> (Left' l')  = Left' l'
  (Right' r) <> (Right' r') = Right' $ r <> r'

instance (Semigroup l, Semigroup r, Monoid r) => Monoid (Either' l r) where
  mempty = Right' mempty
  mappend = (<>)
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
readMatrix = runParser pMatrix


readRectangle :: Coordinate r => Text -> Either Text (Rectangle () r)
readRectangle = runParser pRectangle

-----------------------------------------------------------------------
-- | The parsers themselves


pOperation :: forall r. Coordinate r => Parser (Operation r)
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
pPoint = Point2 <$> pCoordinate <* pWhiteSpace <*> pCoordinate


pCoordinate :: Coordinate r => Parser r
pCoordinate = fromSeq <$> pInteger <*> pDecimal
              where
                pDecimal  = pMaybe (pChar '.' *> pPaddedNatural)


pRectangle :: Coordinate r => Parser (Rectangle () r)
pRectangle = (\p q -> box (ext p) (ext q)) <$> pPoint
                                           <*  pWhiteSpace
                                           <*> pPoint

pMatrix :: Coordinate r => Parser (Matrix 3 3 r)
pMatrix = (\a b -> mkMatrix (a:b)) <$> pCoordinate
                                   <*> pCount 5 (pWhiteSpace *> pCoordinate)


-- | Generate a matrix from a list of 6 coordinates.
mkMatrix               :: Coordinate r => [r] -> Matrix 3 3 r
mkMatrix [a,b,c,d,e,f] = Matrix $ Vector3 (Vector3 a c e)
                                          (Vector3 b d f)
                                          (Vector3 0 0 1)
                           -- We need the matrix in the following order:
                         -- 012
                         -- 345
                         --
                         -- But ipe uses the following order:
                         -- 024
                         -- 135
mkMatrix _             = error "mkMatrix: need exactly 6 arguments"
