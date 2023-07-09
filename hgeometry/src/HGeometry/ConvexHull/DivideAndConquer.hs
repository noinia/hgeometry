{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.ConvexHull.DivideAndConquer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(O(n\log n)\) time divide and conquer algorithm to compute the convex hull
-- of a set of \(n\) points in \(\mathbb{R}^2\).
--
--------------------------------------------------------------------------------
module HGeometry.ConvexHull.DivideAndConquer
  ( convexHull
  , upperHull
  , lowerHull
  ) where

import           HGeometry.Algorithms.DivideAndConquer
import           Control.Arrow ((&&&))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Ext
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Convex.Tangents
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Vector
--------------------------------------------------------------------------------

-- | \(O(n \log n)\) time ConvexHull using divide and conquer. The resulting polygon is
-- given in clockwise order.
convexHull            :: (Ord r, Num r, Point_ point 2 r)
                      => NonEmpty point -> ConvexPolygon point
-- convexHull (p :| []) = ConvexPolygon . unsafeFromPoints $ [p]
convexHull pts       = combine . (upperHull' &&& lowerHull') . NonEmpty.sortBy incXdecY $ pts
  where
    combine (_:|uh,l:|lh) = uncheckedFromCCWPoints $ l :| lh <> reverse uh

----------------------------------------
-- * Computing a lower hull

-- | \(O(n \log n)\) time LowerHull using divide and conquer. The resulting Hull is
-- given from left to right, i.e. in counter clockwise order.
lowerHull :: (Ord r, Num r, Point_ point 2 r)
          => NonEmpty point -> NonEmpty point
lowerHull = lowerHull' . NonEmpty.sortBy incXdecY

lowerHull' :: (Ord r, Num r, Point_ point 2 r) => NonEmpty point -> NonEmpty point
lowerHull' = unLH . divideAndConquer1 (LH . (:|[]))

newtype LH point = LH { unLH :: NonEmpty point } deriving (Eq,Show)

instance (Point_ point 2 r, Num r, Ord r) => Semigroup (LH point) where
  (LH lh) <> (LH rh) = LH $ hull lowerTangent' lh rh

----------------------------------------
-- * Computing an upper hull

-- | \(O(n \log n)\) time UpperHull using divide and conquer. The resulting Hull is
-- given from left to right, i.e. in clockwise order.
upperHull :: (Ord r, Num r, Point_ point 2 r) => NonEmpty point -> NonEmpty point
upperHull = upperHull' . NonEmpty.sortBy incXdecY

upperHull' :: (Ord r, Num r, Point_ point 2 r) => NonEmpty point -> NonEmpty point
upperHull' = unUH . divideAndConquer1 (UH . (:|[]))

newtype UH point = UH { unUH :: NonEmpty point  }

instance ( Point_ point 2 r, Num r, Ord r) => Semigroup (UH point) where
  (UH lh) <> (UH rh) = UH $ hull upperTangent' lh rh

----------------------------------------

-- | The function that does the actual merging part
hull               :: (NonEmpty point -> NonEmpty point -> Vector 2 (point :+ [point]))
                   -> NonEmpty point -> NonEmpty point -> NonEmpty point
hull tangent lh rh = let Vector2 (l :+ lh') (r :+ rh') = tangent (NonEmpty.reverse lh) rh
                     in NonEmpty.fromList $ reverse lh' <> [l,r] <> rh'

--------------------------------------------------------------------------------

incXdecY  :: (Ord r, Point_ point 2 r) => point -> point -> Ordering
incXdecY (Point2_ px py) (Point2_ qx qy) = compare px qx <> compare qy py
