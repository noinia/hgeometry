module Line.BatchPointLocation
  (

    PointLocationDS
  , pointLocationStructure
  , query
  ) where

import HGeometry.Sequence.Alternating
import Data.Vector qualified as Vector
import Data.Set qualified as Set
import HGeometry.VerticalRayShooting.PersistentSweep
import Data.Maybe
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.IntMap.Monoidal qualified as IntMap

--------------------------------------------------------------------------------

type PointLocationDS = ()

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
pointLocationStructure lines = pointLocationStructureFrom inters lines
  where
    inters = undefined


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
