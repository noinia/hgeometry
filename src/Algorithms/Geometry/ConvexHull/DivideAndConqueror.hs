module Algorithms.Geometry.ConvexHull.DivideAndConqueror( convexHull
                                                        , module Types
                                                        ) where

import           Data.Semigroup.Foldable
import           Data.Semigroup
import           Algorithms.Geometry.ConvexHull.Types as Types
import           Control.Lens((^.))
import           Data.BinaryTree
import           Data.Ext
import           Data.Function(on)
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.Geometry.Polygon.Convex as Convex
import qualified Data.List.NonEmpty as NonEmpty

-- | O(n log n) time ConvexHull using divide and conqueror.
convexHull :: (Ord r, Num r)
           => NonEmpty.NonEmpty (Point 2 r :+ p) -> ConvexHull p r
convexHull = unMerge
           . foldMap1 (Merge . ConvexHull . fromPoints . (:[]) . _unElem)
           . asBalancedBinLeafTree
           . NonEmpty.sortBy (compare `on` (^.core))

newtype Merge r p = Merge { unMerge :: ConvexHull p r }

instance (Num r, Ord r) => Semigroup (Merge r p) where
  (Merge lp) <> (Merge rp) = Merge . ConvexHull $ ch
    where
      (ch,_,_) = Convex.merge (lp^.extractHull) (rp^.extractHull)
