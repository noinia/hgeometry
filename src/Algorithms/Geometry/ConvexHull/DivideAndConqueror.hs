module Algorithms.Geometry.ConvexHull.DivideAndConqueror( convexHull
                                                        ) where

import           Control.Lens ((^.))
import           Data.BinaryTree
import           Data.Ext
import           Data.Function (on)
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup
import           Data.Semigroup.Foldable

-- | \(O(n \log n)\) time ConvexHull using divide and conqueror.
convexHull :: (Ord r, Num r)
           => NonEmpty.NonEmpty (Point 2 r :+ p) -> ConvexPolygon p r
convexHull = unMerge
           . foldMap1 (Merge . ConvexPolygon . fromPoints . (:[]) . _unElem)
           . asBalancedBinLeafTree
           . NonEmpty.sortBy (compare `on` (^.core))

newtype Merge r p = Merge { unMerge :: ConvexPolygon p r }

instance (Num r, Ord r) => Semigroup (Merge r p) where
  (Merge lp) <> (Merge rp) = let (ch,_,_) = merge lp rp in Merge ch
