module HGeometry.ConvexHull.R3.Naive.Dual
  ( UpperHull
  , upperHull


  , Facet
  , facets
  ) where

import           Control.Lens
import           Data.Foldable (toList)
import           Data.Foldable1
import           Data.List (find)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
-- import           Data.Map (Map)
-- import qualified Data.Map as Map
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import           Data.Maybe (isNothing)
import           HGeometry.Combinatorial.Util
import           HGeometry.Duality
import           HGeometry.Ext
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection (intersects)
import           HGeometry.Plane.LowerEnvelope
import           HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap (mapWithKeyMerge1)
import           HGeometry.Plane.LowerEnvelope.Naive
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Triangle
import           HGeometry.Vector

--------------------------------------------------------------------------------



type UpperHull point = LowerEnvelope (NonVerticalHyperPlane 3 (NumType point) :+ point)


-- | Computes the upper hull of a set of points in R^3
--
-- \(O(n^4)\)
upperHull :: ( Point_ point 3 r
             , Ord r, Fractional r
             , Foldable1 f, Functor f

             , Show point, Show r, Ord point
             ) => f point -> UpperHull point
upperHull = lowerEnvelope . fmap (\p -> dualHyperPlane p :+ p)


type Facet point = NonEmpty point

-- | Outputs the facets of the upper hull.
facets :: (Ord (NumType point)) => UpperHull point -> [Facet point]
facets = \case
    ParallelStrips _      -> [] -- error "facets: parallel strips; no bounded facets"
    ConnectedEnvelope env -> toList $ toFacet <$> NEMap.elems theVertices
      where
        theVertices = mapWithKeyMerge1 (\h reg -> NEMap.fromList $
                                                  (,NonEmpty.singleton h) <$> verticesOf reg
                                       ) (asMap env)
        verticesOf = \case
          Bounded vertices       -> toNonEmpty vertices
          Unbounded _ vertices _ -> vertices

        toFacet hv = (^.extra) <$> hv
  -- We want all vertices v of the lower envelope; let H_v denote all planes that
  -- intersect in v.
  --
  -- In the primal v is actually a plane; the facet is then defined by all points to the
  -- planes in H_v. So, we want to compute H_v, and then take the convex hull of those
  -- points (in the plane dual  to v)
