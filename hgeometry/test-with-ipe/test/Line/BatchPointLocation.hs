module Line.BatchPointLocation
  (

    PointLocationDS
  , PointLocationDS'
  -- , pointLocationStructure

  , groupQueries
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
import Data.Sequence qualified as Seq
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Prelude hiding (lines)
import Line.PointLocation.Type
import HGeometry.Map.NonEmpty.Monoidal(MonoidalNEMap, singleton)
import Debug.Trace
--------------------------------------------------------------------------------

-- | Given a set of \(n\) query points, and a set of \(r\) lines H computes for each
-- query q the subset of lines above q, in increasing order.
--
-- running time: \(O(n\log n + r^5\log r)\)
groupQueries               :: ( Point_ queryPoint 2 r
                              , Line_ line 2 r
                              , Foldable set
                              , Ord r, Fractional r


                              , Show line, Show r -- FIXME: Remoe theese

                              , Eq line
                              , IsBoxable queryPoint

                              , IsIntersectableWith line (Rectangle (Point 2 r))
                              , Intersection line (Rectangle (Point 2 r)) ~
                                Maybe (LineBoxIntersection 2 r)
                              )
                           => NonEmpty queryPoint
                           -> set line
                           -> MonoidalNEMap (FaceIx (PointLocationDS' r line)) (NonEmpty queryPoint)
groupQueries queries lines | traceShow "groupQueries" False = undefined
groupQueries queries lines = foldMap1 (\q -> singleton (pointLocate q pointLocDS)
                                                       (NonEmpty.singleton q)
                                      ) queries
  where
    bx         = grow 1 $ boundingBox queries
    pointLocDS = traceShowId $
      pointLocationStructureIn bx lines

 -- queries planes = foldMap1 (answerBatch planes)
 --                                    $ groupQueries (imap (\i x -> x :+ i) queries) planes


grow             :: (Num r, Point_ point d r) => r -> Box point -> Box point
grow d (Box p q) = Box (p&coordinates %~ subtract d)
                       (q&coordinates %~ (+d))

--------------------------------------------------------------------------------


-- type PointLocationDS r line =
--   CPlaneGraph () (Point 2 r :+ Seq.Seq (Point 2 r))
--                  (ViewL1 (ClosedLineSegment (Point 2 r) :+ Maybe line))
--                  ()





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







{-

-- | Given a set of n lines, builds a point location data structure.
--
-- \(O(n^2 \log n)\)
pointLocationStructure       :: set line
                             -> PointLocationDS' r line
pointLocationStructure lines = undefined -- pointLocationStructureFrom inters lines

-}

-- | Construct a point location data structure on the given set of n
-- lines in a given bounding rectangle.
--
-- O(n^2 \log n)
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
                                    -> PointLocationDS' r line
pointLocationStructureIn rect lines | traceShow "called" False = undefined
pointLocationStructureIn rect lines = pointLocationStructureFrom gr
  where
    gr :: CPlaneGraph () (Point 2 r)
                         (ClosedLineSegment (Point 2 r) :+ Maybe line) ()
    gr = gr' & vertices %~ view core

    -- | Construct a plane graph
    gr' :: CPlaneGraph () (Point 2 r :+ Seq.Seq (Point 2 r))
                          (ClosedLineSegment (Point 2 r) :+ Maybe line) ()
    gr' =  gr'' & edges %~ view head1

    gr'' :: CPlaneGraph () _ _ _
    gr'' = fromIntersectingSegments segs


    segs :: NonEmpty (ClosedLineSegment (Point 2 r) :+ Maybe line)
    segs = (toNonEmpty $ (:+ Nothing) <$> sides rect)
         <<> mapMaybe clip (toList lines)

    clip l = l `intersect` rect <&> \case
      Line_x_Box_LineSegment seg -> seg :+ Just l
      Line_x_Box_Point _         -> error "pointLocationStructureIn: unhandled"


-- | Builds the point location data structure (by building a structure
-- for vertical ray shooting).
--
-- pre: there the input graph has at least one edge.
--
-- \(O(n\log n)\), where \(n)\ is the number of vertices of the plane graph.
pointLocationStructureFrom    :: ( LineSegment_ edge vertex
                                 , Point_ vertex 2 r
                                 , Ord r, Fractional r
                                 )
                              => CPlaneGraph () vertex edge face
                              -> PointLocationDS vertex edge face
pointLocationStructureFrom gr = PointLocationDS gr vrDS (gr^.outerFace.asIndex)
  where
    vrDS = verticalRayShootingStructure
         . fmap orientLR'
         . NonEmpty.fromList -- Note: by the precondition this is safe.
         $ gr^..edgeSegments.asIndexedExt

    orientLR' e'@(e :+ d) | e^.start.asPoint <= e^.end.asPoint = e'
                          | otherwise                          = e :+ gr^.twinOf d

    -- we want the dart corresponding to the left-to-right oreintation of the segment



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
pointLocate      :: ( Point_ queryPoint 2 r, Num r, Ord r
                    , LineSegment_ edge vertex, Point_ vertex 2 r
                    , HasSupportingLine edge
                    )
                 => queryPoint -> PointLocationDS vertex edge f
                 -> FaceIx (PointLocationDS vertex edge f)
pointLocate q ds = case segmentAbove q (ds^.vrStructure) of
  Nothing       -> ds^.outerFaceIx
  Just (_ :+ d) -> ds^.subdivision.incidentFaceOf d.asIndex




--------------------------------------------------------------------------------


(x :| xs) <<> ys = x :| (xs ++ ys)
