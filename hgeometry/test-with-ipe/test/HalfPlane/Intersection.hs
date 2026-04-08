module HalfPlane.Intersection
  ( leftBoundary
  , rightBoundary
  ) where -- TODO: rename to HGeometry.Slab.Intersection

import           HGeometry.Slab
import           Control.Lens
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Line
import           HGeometry.Box
import           HGeometry.Polygon
import           HGeometry.Vector
import           HGeometry.Vector
import           Prelude hiding (sqrt)
import           HGeometry.Ext
import           HGeometry.Number.Radical
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Cyclic

--------------------------------------------------------------------------------
-- move to Slab.Intersect in hgeometry

-- | Get the left boundary of the slab
leftBoundary   :: Slab r side -> LinePV 2 r :+ side
leftBoundary s = let l = s^.definingLine in l :+ s^.leftData

-- | Get the right boundary of the slab
rightBoundary   :: (Fractional r, Radical r) => Slab r side -> LinePV 2 r :+ side
rightBoundary s = let (LinePV p v@(Vector2 x y)) = s^.definingLine
                      n                          = signorm $ Vector2 y (-x)
                      dist                       = sqrt $ s^.squaredWidth
                      p'                         = p .+^ (dist *^ n)
                        --
                  in (LinePV p' v) :+ s^.rightData

{-
-- TODO: this is actually more annoying
-- since the intersection could be a triangle
-- etc.

-- | turn the slab into a convex polygon by clipping it in the given rectangle.
--
-- pre:  the apex lies inside the given rectangle
toConvexPolygonIn        :: ( Rectangle_ rectangle point
                            , Point_ point 2 r, Ord r, Radical r
                            -- , IsIntersectableWith (HalfLine point) (ClosedLineSegment point)
                            -- , Intersection (HalfLine point) (ClosedLineSegment point)
                            --   ~ Maybe (HalfLineLineSegmentIntersection (Point 2 r)
                            --                                          (ClosedLineSegment point))
                            )
                         => rectangle
                         -> Slab r side
                         -> ConvexPolygonF (Cyclic NonEmpty) (OriginalOrExtra point (Point 2 r))
toConvexPolygonIn rect s = let hp      = leftBoundary  s ^.core
                               hq      = rightBoundary s ^.core
                               rect'   = Box (rect^.minPoint.asPoint) (rect^.maxPoint.asPoint)
                               extrasA = extraPoints hp hq rect'
                               extrasB = extraPoints (hq&direction %~ negated)
                                                     (hp&direction %~ negated)
                                                     rect'
                           in uncheckedFromCCWPoints $
                                Original (c^.apex) NonEmpty.<| (Extra <$> extras)

-}
