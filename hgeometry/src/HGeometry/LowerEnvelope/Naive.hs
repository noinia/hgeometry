module HGeometry.LowerEnvelope.Naive
  ( lowerEnvelope
  , triangulatedLowerEnvelope
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad ((<=<), when)
import qualified Data.List as List
import           Data.Ord
import qualified Data.Vector as Boxed
import           HGeometry.Combinatorial.Util
import           HGeometry.Foldable.Sort
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.LowerEnvelope.Triangulate
import           HGeometry.LowerEnvelope.Type
import           HGeometry.Point
import           HGeometry.Vector
import           Witherable

--------------------------------------------------------------------------------

-- | Brute force implementation of the lower envelope.
--
-- running time: \(O(n^4)\)
lowerEnvelope    :: (Foldable f, Ord r, Fractional r)
                 => f (Plane r) -> LowerEnvelope [] Boxed.Vector r
lowerEnvelope hs = LowerEnvelope vertices halfEdges
  where
    vertices = mapMaybe (guarded (`belowAll` hs) <=< asVertex)
             $ uniqueTriplets hs

    halfEdges = sortBy aroundOrigins . mapMaybe asHalfEdge
              $ uniquePairs vertices

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

--
guarded                 :: Alternative f => (a -> Bool) -> a -> f a
guarded p v | p v       = empty
            | otherwise = pure v
