--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.VertexForm
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- The vertices of the lower envelope; i.e. the lower envelope in VertexForm
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.VertexForm
  ( VertexForm
  , Definers(..)
  , fromCCWList
  , definers
  ) where

import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import           Data.Maybe (fromMaybe)
import           HGeometry.Combinatorial.Util
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Plane.LowerEnvelope.Connected.Primitives
import           HGeometry.Point
import           HGeometry.Vector

--------------------------------------------------------------------------------
-- *  The planes defining a vertex

-- | The vertices of a lower envelope is just a Map with every vertex its definers,
-- i.e. the planes that define the vertex in CCW order around it.
type VertexForm r plane = Map (Point 3 r) (Definers plane)

----------------------------------------
-- *  The planes defining a vertex

-- | in CCW order, starting with the plane that is minimal at the vertical up direction
-- from their common vertex.
newtype Definers plane = Definers (NonEmpty plane)
  deriving stock (Show,Eq,Ord)
  deriving newtype (Functor,Foldable,Foldable1)

-- | Given the planes in order, starting with the one that is closest in the up direction,
-- construct the Definers.
fromCCWList :: NonEmpty plane -> Definers plane
fromCCWList = Definers

-- | Given the planes in order, starting with the one that is closest in the up direction,
-- construct the Definers.
fromCCWList' :: [plane] -> Definers plane
fromCCWList' = fromCCWList . NonEmpty.fromList

-- | Smart constructor for creating the definers of three planes
definers                                    :: forall plane r.(Plane_ plane r, Ord r, Fractional r)
                                            => Three plane -> Maybe (Point 3 r, Definers plane)
definers (Three h1@(Plane_ a1 b1 c1) h2 h3) =
    do l12 <- intersectionLine h1 h2
       l13 <- intersectionLine h1 h3
       intersect l12 l13 >>= \case
         Line_x_Line_Line _             -> Nothing
         Line_x_Line_Point (Point2 x y) -> Just ( Point3 x y (a1 * x + b1* y + c1)
                                                , fromCCWList' [hMin, hTwo, hThree]
                                                )
           where
             (hMin,h,h')   = extractMinOn (evalAt $ Point2 x (y+1)) h1 h2 h3
             -- we compute the plane hMin that is cheapest directly above the vertex h and
             -- h' are the other two planes. That Means hMin is the first definer (in the
             -- CCW order). What remains is to determine the order in which the remaining
             -- planes h and h' appear in the order.
             (hTwo,hThree) = case ccwCmpAroundWith (Vector2 0 1) (origin :: Point 2 r)
                                                   (Point vMinH) (Point vMinH') of
                               LT -> (h,h')
                               EQ -> error "definers: weird degeneracy?"
                               GT -> (h',h)

             vMinH  = fromMaybe err $ intersectionVector h  hMin
             vMinH' = fromMaybe err $ intersectionVector h' hMin
             err = error "definers: absurd"

-- | given three elements, returns the minimum element in the first argument and the
-- remaining two elements in the second and third argument (in arbitrary order).
extractMinOn         :: Ord a => (c -> a) -> c -> c -> c -> (c, c, c)
extractMinOn f a b c = let (m,ab)  = min' a b
                           (mi,c') = min' m c
                       in (mi, ab, c')
  where
    min' x y
      | f x <= f y = (x,y)
      | otherwise  = (y,x)
