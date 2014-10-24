{-# Language FlexibleInstances #-}
module Data.Geometry.Ipe.PathParser where

import Numeric

import Control.Applicative
import Control.Monad

import Data.Char(isSpace)
import Data.Ratio
import Data.List.Split

import Data.Geometry.Point

import Data.Geometry.Ipe.Types
import Data.Geometry.Ipe.ParserPrimitives


-----------------------------------------------------------------------
-- | Represent stuff that can be used as a coordinate in ipe. (similar to show/read)

class Coordinate r where
    toIpeOut :: r -> String
    fromSeq            :: Num r => Integer -> Maybe Integer -> r
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

readPoint   :: Coordinate r => String -> Maybe (Point 2 r)
readPoint s = case runP' pPoint s of
                Left  _     -> Nothing
                Right (p,_) -> Just p

readPathOperations :: Coordinate r => String -> [Operation r]
readPathOperations = map (fst . runP pOperation) . clean . splitP
    where
      -- Split the input string in pieces, each piece represents one operation
      splitP = split . keepDelimsR $ oneOf "mlcqeasuh"
      trim   = dropWhile isSpace
      clean  = filter (not . null) . map trim


readMatrix :: Coordinate r => String -> Matrix 3 r
readMatrix = fst . runP pMatrix

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
pPoint = curry Point2 <$> pCoordinate <* pWhiteSpace <*> pCoordinate


pCoordinate :: Coordinate r => Parser r
pCoordinate = fromSeq <$> pInteger <*> pDecimal
              where
                pDecimal = pMaybe (pChar '.' *> pInteger)

pMatrix :: Coordinate r => Parser (Matrix 3 r)
pMatrix = (\a b -> mkMatrix (a:b)) <$> pCoordinate
                                   <*> pCount 5 (pWhiteSpace *> pCoordinate)


-- TODO
matrix3FromLists = undefined


-- | Generate a matrix from a list of 6 coordinates.
mkMatrix               :: Coordinate r => [r] -> Matrix 3 r
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
