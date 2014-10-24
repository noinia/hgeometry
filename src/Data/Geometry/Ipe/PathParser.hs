{-# Language FlexibleInstances #-}
module Data.Geometry.Ipe.PathParser where

import Numeric

import Control.Applicative
import Control.Monad

import Data.Char(isSpace)
import Data.Ratio
import Data.List.Split

import Data.Geometry.Point
import Data.Geometry.Geometry

import Data.Geometry.Ipe.InternalTypes
import Data.Geometry.Ipe.ParserPrimitives


-----------------------------------------------------------------------
-- | Represent stuff that can be used as a coordinate in ipe. (similar to show/read)

class Coordinate a where
    toIpeOut :: a -> String
    fromSeq            :: Num a => Integer -> Maybe Integer -> a
    fromSeq x Nothing  = fromInteger x
    fromSeq x (Just y) = let x'        = fromInteger x
                             y'        = fromInteger y
                             asDecimal = head . dropWhile (>= 1) . iterate (* 0.1) in
                         signum x' * (abs x' + asDecimal y')

instance Coordinate Double where
    toIpeOut = show

instance Coordinate (Ratio Integer) where
    toIpeOut r = show (fromRat r :: Double)
    fromSeq x  Nothing = fromInteger x
    fromSeq x (Just y) = fst . head $ readSigned readFloat (show x ++ "." ++ show y)

-----------------------------------------------------------------------
-- | Running the parsers

readPoint   :: Coordinate a => String -> Maybe (Point2' a)
readPoint s = case runP' pPoint s of
                Left  _     -> Nothing
                Right (p,_) -> Just p

readPathOperations :: Coordinate a => String -> [Operation a]
readPathOperations = map (fst . runP pOperation) . clean . splitP
    where
      -- Split the input string in pieces, each piece represents one operation
      splitP = split . keepDelimsR $ oneOf "mlcqeasuh"
      trim   = dropWhile isSpace
      clean  = filter (not . null) . map trim


readMatrix :: Coordinate a => String -> Matrix3 a
readMatrix = fst . runP pMatrix

-----------------------------------------------------------------------
-- | The parsers themselves


pOperation :: Coordinate a => Parser (Operation a)
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


pPoint :: Coordinate a => Parser (Point2' a)
pPoint = curry Point2 <$> pCoordinate <* pWhiteSpace <*> pCoordinate


pCoordinate :: Coordinate a => Parser a
pCoordinate = fromSeq <$> pInteger <*> pDecimal
              where
                pDecimal = pMaybe (pChar '.' *> pInteger)

pMatrix :: Coordinate a => Parser (Matrix3 a)
pMatrix = (\a b -> mkMatrix (a:b)) <$> pCoordinate
                                   <*> pCount 5 (pWhiteSpace *> pCoordinate)


-- | Generate a matrix from a list of 6 coordinates.
mkMatrix               :: Coordinate a => [a] -> Matrix3 a
mkMatrix [a,b,c,d,e,f] = matrix3FromLists [ [a, c, e]
                                          , [b, d, f]
                                          , [0, 0, 1] ]
                         -- We need the matrix in the following order:
                         -- 012
                         -- 345
                         --
                         -- But ipe uses the following order:
                         -- 024
                         -- 135
