module HGeometry.LowerEnvelope.Naive
  ( lowerEnvelope
  , triangulatedLowerEnvelope
  , belowAll
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad ((<=<))
import qualified Data.Vector as Boxed
import           HGeometry.Combinatorial.Util
import           HGeometry.Foldable.Sort
import           HGeometry.HyperPlane.Class
import           HGeometry.LowerEnvelope.Triangulate
import           HGeometry.LowerEnvelope.Type
import           HGeometry.Point
import           Witherable

--------------------------------------------------------------------------------

-- | Brute force implementation of the lower envelope.
--
-- running time: \(O(n^4)\)
lowerEnvelope    :: (Foldable f, Ord r, Fractional r)
                 => f (Plane r) -> LowerEnvelope [] Boxed.Vector r
lowerEnvelope hs = LowerEnvelope vertices' halfEdges'
  where
    vertices' = mapMaybe (guarded (`belowAll` hs) <=< asVertex)
              $ uniqueTriplets hs

    halfEdges' = sortBy aroundOrigins . mapMaybe asHalfEdge
               $ uniquePairs vertices'

    -- TODO: how do we represent the unbounded edges; I guess we
    -- should have one half-edge pointing to the unbounded vertex


-- | Triangulated version of a lower envelope
--
-- running time: \(O(n^4)\)
triangulatedLowerEnvelope :: (Foldable f, Ord r, Fractional r)
                          => f (Plane r) -> LowerEnvelope [] Boxed.Vector r
triangulatedLowerEnvelope = triangulate . lowerEnvelope


--------------------------------------------------------------------------------

-- | Test if the given vertex lies on or below all the given planes
--
-- \(O(n)\)
belowAll   :: (Foldable f, Num r, Ord r) => Vertex r -> f (Plane r) -> Bool
belowAll v = all ((v^.location) `notAbove`)

-- | Returns True when the point is not above the plane, in otherwords
-- when the plane actually passes above (or through) the point.
notAbove                  :: (Ord r, Num r) => Point 3 r -> Plane r -> Bool
notAbove (Point3 x y z) h = not $ z > evalAt (Point2 x y) h

--------------------------------------------------------------------------------



-- horizontalPlane c = Plane 0 0 c



--------------------------------------------------------------------------------

-- | Given an element and a predicate, return the element only if the
-- predicate is true.
guarded                 :: Alternative f => (a -> Bool) -> a -> f a
guarded p v | p v       = pure v
            | otherwise = empty
