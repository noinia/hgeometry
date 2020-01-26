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
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Util
--------------------------------------------------------------------------------

-- | \(O(n \log n)\) time ConvexHull using divide and conquer. The resulting polygon is
-- given in clockwise order.
convexHull           :: (Ord r, Num r) => NonEmpty (Point 2 r :+ p) -> ConvexPolygon p r
convexHull (p :| []) = ConvexPolygon . fromPoints $ [p]
convexHull pts       = combine . (upperHull' &&& lowerHull') . NonEmpty.sortBy incXdecY $ pts
  where
    combine (l:|uh,_:|lh) = ConvexPolygon . fromPoints $ l : uh <> reverse (init lh)

----------------------------------------
-- * Computing a lower hull

-- | \(O(n \log n)\) time LowerHull using divide and conquer. The resulting Hull is
-- given from left to right, i.e. in counter clockwise order.
lowerHull :: (Ord r, Num r)
          => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
lowerHull = lowerHull' . NonEmpty.sortBy incXdecY

lowerHull' :: (Ord r, Num r) => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
lowerHull' = unLH . divideAndConquer1 (LH . (:|[]))

newtype LH r p = LH { unLH :: NonEmpty (Point 2 r :+ p) } deriving (Eq,Show)

instance (Num r, Ord r) => Semigroup (LH r p) where
  (LH lh) <> (LH rh) = LH $ hull lowerTangent' lh rh

----------------------------------------
-- * Computing an upper hull

-- | \(O(n \log n)\) time UpperHull using divide and conquer. The resulting Hull is
-- given from left to right, i.e. in clockwise order.
upperHull :: (Ord r, Num r) => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
upperHull = upperHull' . NonEmpty.sortBy incXdecY

upperHull' :: (Ord r, Num r) => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
upperHull' = unUH . divideAndConquer1 (UH . (:|[]))

newtype UH r p = UH { unUH :: NonEmpty (Point 2 r :+ p) }

instance (Num r, Ord r) => Semigroup (UH r p) where
  (UH lh) <> (UH rh) = UH $ hull upperTangent' lh rh

----------------------------------------

-- | The function that does the actual merging part
hull               :: (NonEmpty p -> NonEmpty p -> Two (p :+ [p]))
                   -> NonEmpty p -> NonEmpty p -> NonEmpty p
hull tangent lh rh = let Two (l :+ lh') (r :+ rh') = tangent (NonEmpty.reverse lh) rh
                     in NonEmpty.fromList $ (reverse lh') <> [l,r] <> rh'

--------------------------------------------------------------------------------

incXdecY  :: Ord r => (Point 2 r) :+ p -> (Point 2 r) :+ q -> Ordering
incXdecY (Point2 px py :+ _) (Point2 qx qy :+ _) =
  compare px qx <> compare qy py
