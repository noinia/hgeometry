module Line.BatchPointLocation
  (

    PointLocationDS
  , pointLocationStructure
  , query
  ) where

import Data.Foldable
import Data.Foldable1
import Control.Lens
import HGeometry.Sequence.Alternating
import Data.Vector qualified as Vector
import Data.Set qualified as Set
import HGeometry.VerticalRayShooting.PersistentSweep
import Data.Maybe
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.IntMap.Monoidal qualified as IntMap
import HGeometry.PlaneGraph.Connected
import HGeometry.Sequence.NonEmpty
import HGeometry.Ext
import HGeometry.Kernel
import HGeometry.Box
import Data.Sequence as Seq
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Prelude hiding (lines)

--------------------------------------------------------------------------------

type PointLocationDS r line =
  CPlaneGraph () (Point 2 r :+ Seq.Seq (Point 2 r))
                 (ViewL1 (ClosedLineSegment (Point 2 r) :+ Maybe line))
                 ()





-- -- | Given a set of lines; split it into interior disjoint linesegments
-- splitIntoSegments          :: Sides r
--                               -- ^ initial box
--                            -> set line
--                            -> [ClosedLineSegment (Point 2 r) :+ line]
-- splitIntoSegments bx lines = undefined
--   where
--     -- For each line (by index)
--     verticesPerLine :: IntMap (Map (Point 2 r) [Point 2 r])
--     verticesPerLine = foldMap asVertex $ uniquePairs (zip [0..] lines)
--     asVertex (Two (i, l1) (j, l2)) = case l1 `intersect` l2 of
--         Just (Line_x_Line_Point v) -> IntMap.fromList [ (i, Map.singleton v [v])
--                                                       , (j, Map.singleton v [v])
--                                                       ]
--         Nothing                    -> mempty

--     thePoints = foldMap Map.keySet verticesPerLine
--     maxY = maximumByOf folded (comparing (^.yCoord)) thePoints
--     minY = minimumByOf folded (comparing (^.yCoord)) thePoints





    -- bx = Sides max' max' min' min'
    --      <$> bx <*> Sides maxY ((^.xCoord) <$> Set.maxView thePoints)
    --                       minY ((^.xCoord) <$> Set.minView thePoints)
    -- max' a = maybe a (max a)
    -- min' a = maybe a (min a)

    -- Point2 minX _ = minView thePoints
    -- Point2 minX _ = minView thePoints



    -- box =

    --   boundingBox verticesPerLine


-- instance Dimension (IntMap.IntMap v) = Dimension v
-- instance NumType   (IntMap.IntMap v) = NumType v

-- instance Dimension (Map.Map k v) = Dimension v
-- instance NumType   (Map.Map k v) = NumType v


-- instance IsBoxable v => IsBoxable (IntMap.IntMap k v) where
--   boundingBox = foldMap boundingBox

-- instance IsBoxable v => IsBoxable (Map.Map k v) where
--   boundingBox = foldMap boundingBox








-- newtype PointLocationDS r line =
--   PointLocationDS (Alternating Vector.Vector r (Set.Set line))


-- | Given a set of n lines, builds a point location data structure.
--
-- \(O(n^2 \log n)\)
pointLocationStructure       :: set line -> PointLocationDS r line
pointLocationStructure lines = undefined -- pointLocationStructureFrom inters lines
  where
    inters = undefined


-- | Construct a point location data structure in a given bounding rectangle.
pointLocationStructureIn            :: forall set line r.
                                       (Foldable set , Line_ line 2 r, Fractional r, Ord r
                                       , Eq line

                                       , IsIntersectableWith line (Rectangle (Point 2 r))
                                       , Intersection line (Rectangle (Point 2 r)) ~
                                         Maybe (LineBoxIntersection 2 r)
                                       )
                                    => Rectangle (Point 2 r)
                                       -- ^ bounding rectangle
                                    -> set line
                                    -> PointLocationDS r line
pointLocationStructureIn rect lines = pointLocationStructureFrom gr
  where
    -- | Construct a plane graph
    gr :: CPlaneGraph () _ _ _
    gr = fromIntersectingSegments segs

    segs :: NonEmpty (ClosedLineSegment (Point 2 r) :+ Maybe line)
    segs = (toNonEmpty $ (:+ Nothing) <$> sides rect)
         <<> mapMaybe clip (toList lines)

    clip l = l `intersect` rect <&> \case
      Line_x_Box_LineSegment seg -> seg :+ Just l
      Line_x_Box_Point _         -> error "pointLocationStructureIn: unhandled"


pointLocationStructureFrom = id


-- pointLocationStructureFrom         :: Intersections r lineSegment
--                                    -> set lineSegment
--                                    -> PointLocationDS r lineSegment
-- pointLocationStructureFrom ix segs = sweep eventw




-- | Given a query point, and the structure
--
-- reports the interval/slab containing the query point, and the line
-- directly above the query point (if such a line exists).
--
-- \(O(\log n)\)
-- query :: queryPoint -> PointLocationDS r line -> (Interval (Unbounded r), Maybe line)
query = undefined

--------------------------------------------------------------------------------


(x :| xs) <<> ys = x :| (xs ++ ys)
