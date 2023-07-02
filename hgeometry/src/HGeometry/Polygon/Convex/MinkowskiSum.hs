module HGeometry.Polygon.Convex.MinkowskiSum
  ( minkowskiSum
  ) where

import           Control.Lens
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           HGeometry.Ext
import           HGeometry.Point
import           HGeometry.Point.Class
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex.Class
import           HGeometry.Polygon.Convex.Implementation
import           HGeometry.Polygon.Simple.Class
import           Hiraffe.Graph

import Data.Default.Class
--------------------------------------------------------------------------------


-- | Computes the Minkowski sum of the two input polygons with $n$ and $m$
-- vertices respectively.
--
-- running time: \(O(n+m)\).
minkowskiSum     :: forall convexPolygon convexPolygon' point point' r.
                    ( Ord r, Num r
                    , ConvexPolygon_ convexPolygon  point r
                    , ConvexPolygon_ convexPolygon' point' r
                    , Default point'
                    )
                 => convexPolygon -> convexPolygon'
                 -> ConvexPolygon (point :+ point')
minkowskiSum p q = uncheckedFromCCWPoints $ merge' (f p) (f q)
  where
    -- f    :: (Polygon_ polygon myPoint r) => polygon -> [myPoint]
    f p' = let (v:|xs) = toNonEmptyOf (ccwOuterBoundaryFrom $ bottomMost p') p'
           in v: xs++[v]

    v .+. w = v .+^ (w^.vector) :+ w

    cmpAngle v v' w w' =
      ccwCmpAround origin (Point $ v' .-. v) (Point $ w' .-. w)

    merge' [_]       [_]       = []
    merge' vs@[v]    (w:ws)    = v .+. w : merge' vs ws
    merge' (v:vs)    ws@[w]    = v .+. w : merge' vs ws
    merge' (v:v':vs) (w:w':ws) = v .+. w :
      case cmpAngle v v' w w' of
        LT -> merge' (v':vs)   (w:w':ws)
        GT -> merge' (v:v':vs) (w':ws)
        EQ -> merge' (v':vs)   (w':ws)
    merge' _         _         = error "minkowskiSum: Should not happen"


bottomMost :: (Polygon_ polygon point r, Ord r) => polygon -> VertexIx polygon
bottomMost = fst . first1Of (minimumVertexBy cmp . withIndex)
  where
    cmp p q = comparing (^.yCoord) p q <> comparing (^.xCoord) p q


-- newtype UnsafeExtra extra = UnsafeExtra extra

-- instance {-# OVERLAPS #-} (Point_ point d r) => Point_ (point :+ UnsafeExtra extra) d r where
--   fromVector v = fromVector v :+ undefined
