{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.CatmulRomSpline
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Catmul Rom Splines
--
--------------------------------------------------------------------------------
module Data.Geometry.CatmulRomSpline where

import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.BezierSpline
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.LSeq (LSeq)
import qualified Data.LSeq as LSeq
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))

import Data.RealNumber.Rational

--------------------------------------------------------------------------------

-- | Catmul-Rom Spline.
newtype Spline d r = Spline (LSeq 4 (Point d r))

deriving instance (Arity d, Show r) => Show (Spline d r)
deriving instance (Arity d, Eq r)   => Eq (Spline d r)

deriving instance Arity d => Functor (Spline d)

-- | Constructs a spline from four points.
fromPoints              :: Point d r -> Point d r -> Point d r -> Point d r -> Spline d r
fromPoints p1 p2 p3 p4 = fromListOfPointsUnsafe [p1,p2,p3,p4]

-- | Constructs a spline from a list of, at least four, points..
fromListOfPoints :: [Point d r] -> Maybe (Spline d r)
fromListOfPoints = fmap Spline . LSeq.eval (C @4) . LSeq.fromList

fromListOfPointsUnsafe :: [Point d r] -> Spline d r
fromListOfPointsUnsafe = Spline . LSeq.promise . LSeq.fromList


-- | Converts a CatmulRom Spline into a series of Cubic Bezier curves.
toCubicBezier  :: (Arity d, Fractional r) => Spline d r -> NonEmpty (BezierSpline 3 d r)
toCubicBezier (Spline (toList -> pts)) =
    NonEmpty.fromList $ List.zipWith4 f pts (drop 1 pts) (drop 2 pts) (drop 3 pts)
    -- note that since the spline is guaranteed to contain at least four points, the
    -- drop's are safe, and so is  the conversion into a NonEmpty list.
  where
    f p1 p2 p3 p4 =
      Bezier3 p2 (p2 .+^ ((p1 .-. p3) ^/ (6*tau))) (p3 .-^ ((p4 .-. p2) ^/ (6*tau))) p3

    -- the tension
    tau = 1 / 2
  -- using: https://pomax.github.io/bezierinfo/#catmullconv


type R = RealNumber 10

test :: Spline 2 R
test = Spline . LSeq.promise . LSeq.fromList $ [origin, Point2 10 10, Point2 10 0, Point2 20 1]
