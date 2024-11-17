module HGeometry.ConvexHull.R3.Naive.Dual
  ( UpperHull
  , upperHull


  , Facet
  , facets
  ) where

import Control.Lens
import Data.Foldable (toList)
import Data.Foldable1
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isNothing)
import HGeometry.Combinatorial.Util
import HGeometry.Duality
import HGeometry.Ext
import HGeometry.HalfSpace
import HGeometry.HyperPlane
import HGeometry.HyperPlane.NonVertical
import HGeometry.Intersection (intersects)
import HGeometry.Plane.LowerEnvelope
import HGeometry.Plane.LowerEnvelope.Naive
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Triangle
import HGeometry.Vector

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


type Facet point = [point]

-- | Outputs the facets of the upper hull.
facets :: UpperHull point -> [Facet point]
facets = \case
    ParallelStrips _      -> error "facets: parallel strips; no bounded facets"
    ConnectedEnvelope env -> undefined

  --   toPlaneGraph' env


  --     env^..boundedVertices.traverse.to toFacet
  -- where
  --   toFacet   :: BoundedVertexF f (plane :+ point) -> Facet point
  --   toFacet v = (^.extra) <$> v^.definers.to toList
