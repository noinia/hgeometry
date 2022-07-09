{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.ConvexHull.DivideAndConquer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(O(n\log n)\) time divide and conquer algorithm to compute the convex hull
-- of a set of \(n\) points in \(\mathbb{R}^2\).
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.ConvexHull.DivideAndConquer( convexHull
                                                      , upperHull
                                                      , lowerHull
                                                      ) where

import           Algorithms.DivideAndConquer
import           Control.Arrow ((&&&))
import           Control.Lens (view)
import           Data.Ext
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Util
import           Geometry.Point
import           Geometry.Polygon
import           Geometry.Polygon.Convex
--------------------------------------------------------------------------------

-- | \(O(n \log n)\) time ConvexHull using divide and conquer. The resulting polygon is
-- given in clockwise order.
convexHull           :: forall point r.
                        ( Ord r, Num r, Point_ point 2 r
                        , AsExt (point 2 r)
                        , CoreOf (point 2 r) ~ Point 2 r
                        )
                     => NonEmpty (point 2 r) -> ConvexPolygon (ExtraOf (point 2 r)) r
convexHull (p :| []) = mkConvexPolygon [p]
convexHull pts       = combine . (upperHull' &&& lowerHull') . NonEmpty.sortBy incXdecY $ pts
  where
    combine (l:|uh,_:|lh) = mkConvexPolygon $ l : uh <> reverse (init lh)

-- | constructs the convex polygon
mkConvexPolygon :: ( AsExt (point 2 r), Point_ point 2 r
                   , CoreOf (point 2 r) ~ Point 2 r
                   ) => [point 2 r] -> ConvexPolygon (ExtraOf (point 2 r)) r
mkConvexPolygon = ConvexPolygon . unsafeFromPoints . map (view _Ext)

----------------------------------------
-- * Computing a lower hull

-- | \(O(n \log n)\) time LowerHull using divide and conquer. The resulting Hull is
-- given from left to right, i.e. in counter clockwise order.
lowerHull :: forall point r. (Ord r, Num r, Point_ point 2 r)
          => NonEmpty (point 2 r) -> NonEmpty (point 2 r)
lowerHull = lowerHull' . NonEmpty.sortBy incXdecY

lowerHull' :: (Ord r, Num r, Point_ point 2 r) => NonEmpty (point 2 r) -> NonEmpty (point 2 r)
lowerHull' = unLH . divideAndConquer1 (LH . (:|[]))

newtype LH r point = LH { unLH :: NonEmpty (point 2 r) }

instance (Num r, Ord r, Point_ point 2 r) => Semigroup (LH r point) where
  (LH lh) <> (LH rh) = LH $ hull lowerTangent' lh rh

----------------------------------------
-- * Computing an upper hull

-- | \(O(n \log n)\) time UpperHull using divide and conquer. The resulting Hull is
-- given from left to right, i.e. in clockwise order.
upperHull :: (Ord r, Num r, Point_ point 2 r) => NonEmpty (point 2 r) -> NonEmpty (point 2 r)
upperHull = upperHull' . NonEmpty.sortBy incXdecY

upperHull' :: (Ord r, Num r, Point_ point 2 r) => NonEmpty (point 2 r) -> NonEmpty (point 2 r)
upperHull' = unUH . divideAndConquer1 (UH . (:|[]))

newtype UH r point = UH { unUH :: NonEmpty (point 2 r) }

instance (Num r, Ord r, Point_ point 2 r) => Semigroup (UH r point) where
  (UH lh) <> (UH rh) = UH $ hull upperTangent' lh rh

----------------------------------------

-- | The function that does the actual merging part
hull               :: (NonEmpty p -> NonEmpty p -> Two (p :+ [p]))
                   -> NonEmpty p -> NonEmpty p -> NonEmpty p
hull tangent lh rh = let Two (l :+ lh') (r :+ rh') = tangent (NonEmpty.reverse lh) rh
                     in NonEmpty.fromList $ reverse lh' <> [l,r] <> rh'

--------------------------------------------------------------------------------

incXdecY  :: (Ord r, Point_ point 2 r) => point 2 r -> point 2 r -> Ordering
incXdecY (Point2 px py) (Point2 qx qy) = compare px qx <> compare qy py
