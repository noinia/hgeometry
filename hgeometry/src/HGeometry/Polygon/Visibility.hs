{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Visibility
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Computing Visibility
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Visibility
  ( visiblePointsFrom
  , visibilityGraph
  ) where

import           Control.Lens
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Foldable1
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Ord (comparing)
import           Data.Proxy
import           Data.Reflection
import qualified Data.Set as Set
import qualified Data.Vector as V
import           HGeometry.Algorithms.DivideAndConquer (mergeSortedListsBy)
import           HGeometry.Ext
import           HGeometry.Foldable.Sort
import           HGeometry.HalfLine
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.LineSegment ( LineSegment_, start, end
                                       , ClosedLineSegment
                                       , LineLineSegmentIntersection(..)
                                       , HalfLineLineSegmentIntersection(..)
                                       )
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Polygon.Simple.Implementation
import           HGeometry.Polygon.Simple.InPolygon
import           HGeometry.Polygon.Simple.Type
import           HGeometry.Properties
import qualified HGeometry.Set.Util as Set
import           HGeometry.Vector
import           Unsafe.Coerce (unsafeCoerce)
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder
import           Witherable

--------------------------------------------------------------------------------

newtype CCWAround c point = CCWAround { unCCWAround :: point }
  deriving newtype (Show,Eq)

instance ( Reifies c (Point 2 r, Vector 2 r)
         , Point_ point 2 r, Eq point, Ord r, Num r
         ) => Ord (CCWAround c point) where
  (CCWAround p) `compare` (CCWAround q) = let (c,v) = reflect (Proxy @c) in
    ccwCmpAroundWith v c p q  <> cmpByDistanceTo c p q

-- | Construct some ray shooting structure around the center point
data VisibilityStructure center point segment = VisibilityStructure
    { _centerPt         :: !center
    , _initialDirection :: !(Vector 2 (NumType center))
    , _theMap           :: !(Either segment
                                    (Map.Map (CCWAround () (Point 2 (NumType center)))
                                             (point, segment))
                            )
    -- ^ invariant: If there are no points we are at a Left, otherwise at a Right. So, the
    -- Map in the right is never empty.
    --
    -- if there are no points, we hit some 'seg' stored in the Left at infinity.
    --
    -- Otherwise, the points p_1...,p_n in the map are ordered CCW around the center,
    -- starting from the initial direction. For any angle theta in the angular interval
    -- around the center defined by (p_i,p_{i+1}), a ray starting in the center going in
    -- that direction would it segment first. For alpha = the angle corresponding to p_i,
    -- p_i would be the closest point.
    }

-- instance Functor (VisibilityStructure center point) where
-- instance Foldable (VisibilityStructure center point) where
-- instance Traversable (VisibilityStructure center point) where
--   VisibilityStructure ()


--------------------------------------------------------------------------------

coerceMap   :: Proxy c -> Map.Map (CCWAround c' point) seg -> Map.Map (CCWAround c point) seg
coerceMap _ = unsafeCoerce
-- we only use this to inject the center and direction into the Ord

-- | Returns the predecessor point p_i and the segment that we hit (if there are any
-- points), or just our segment at infinity otherwise.
queryVisibilityStructure :: ( Point_ point 2 r
                            , Point_ center 2 r
                            , Point_ query 2 r
                            , Num r, Ord r, Eq point
                            )
                         => query
                         -> VisibilityStructure center point segment
                         -> Either segment (point, segment)
queryVisibilityStructure q (VisibilityStructure c v em) = em <&> \m ->
  reify (c^.asPoint,v) $ \proxy -> case Map.lookupLE (CCWAround $ q^.asPoint)
                                                     (coerceMap proxy m) of
      Nothing  -> case Map.lookupMax m of
                    Nothing -> error "queryVisibilityStructure. absurd"-- map is empty
                    Just res -> snd res
      Just res -> snd res

--------------------------------------------------------------------------------



-- | Computes the closest endpoint among all segments
closestEndPoint  :: (LineSegment_ lineSegment point, Point_ point 2 r, Point_ center 2 r
                    , Ord r, Num r, Foldable1 nonEmpty
                    , HasSquaredEuclideanDistance point
                    ) => center -> nonEmpty lineSegment -> point
closestEndPoint c = minimumBy (cmpByDistanceTo c) . foldMap1 (\s -> s^.start :| [s^.end])



--------------------------------------------------------------------------------




-- | Algorithm to compute the visibilityGraph of a simple polygon
--
-- O(n^3) (for now )
visibilityGraph     :: ( SimplePolygon_ simplePolygon point r
                       , HasIntersectionWith point simplePolygon
                       , Ord r, Fractional r
                       , HasSquaredEuclideanDistance point
                       , HalfLine (Point 2 r) `IsIntersectableWith` ClosedLineSegment point
                       , Intersection (HalfLine (Point 2 r)) (ClosedLineSegment point)
                         ~ Maybe (HalfLineLineSegmentIntersection (Point 2 r)
                                 (ClosedLineSegment point))
                       )
                    => simplePolygon -> [Vector 2 (VertexIx simplePolygon)]
visibilityGraph pg  = (uncurry Vector2 <$> pg^..outerBoundaryEdges.asIndex)
                      <> mapMaybe liesInsidePolygon candidateEdges
  where
    candidateEdges = ifoldMapOf vertices edgesFromV pg

    edgesFromV i v = (Vector2 (v :+ i))
                  <$> visiblePointsFrom v (pg^..outerBoundaryEdgeSegments)
                                          (toExt <$> pg^..vertices.withIndex)

    liesInsidePolygon (Vector2 (u :+ i) (v :+ j))
      | u .+^ ((v .-. u) ^/ 2) `intersects` pg = Just (Vector2 i j)
      | otherwise                              = Nothing


-- asIndexedExt   :: (Indexable i p, Functor f)
--                => p (i, s) (f (t :+ j))
--                -> Indexed i s (f t)
-- asIndexedExt f = Indexed $ \i a -> snd <$> indexed f i (i, a)
-- {-# INLINE asIndexedExt #-}

-- asExtPoint = to $ \(i,p) -> p :+ i
toExt (i,p) = p :+ i


-- | O((n+m)\log (n+m))
--
-- pre; the obstacle edges are pairewise disjoint (except for possibly at endpoints)
visiblePointsFrom :: forall point r queryPoint obstacleEdge endPoint f g.
                     ( Point_ point 2 r, Point_ endPoint 2 r
                     , Point_ queryPoint 2 r
                     , Ord r, Num r
                     , LineSegment_ obstacleEdge endPoint
                     , HasSupportingLine obstacleEdge
                     , HasSquaredEuclideanDistance obstacleEdge
                     , Foldable f, Witherable f, Foldable g
                     , LinePV 2 r `IsIntersectableWith` obstacleEdge
                     , Intersection (LinePV 2 r) obstacleEdge ~
                       Maybe (LineLineSegmentIntersection obstacleEdge)
                     , IsIntersectableWith (HalfLine (Point 2 r)) obstacleEdge
                     , Intersection (HalfLine (Point 2 r)) obstacleEdge
                       ~ Maybe (HalfLineLineSegmentIntersection (Point 2 r) obstacleEdge)
                     )
                  => point -> f obstacleEdge -> g queryPoint -> [queryPoint]
visiblePointsFrom p' obstacleEdges queryPoints = case F.toList queryPoints of
  []       -> []
  (q0 : _) -> snd $ foldr (handle p) (initial,[]) events -- run the sweep
    where
      p   = p'^.asPoint :: Point 2 r
      v   = (q0^.asPoint) .-. p
      ray = HalfLine p v

      -- sort the events cyclically around p
      events = mergeSortedListsBy eventCmp queryEvents endPointEvents

      queryEvents    = sortEvents $ foldMap toEvent  queryPoints
      endPointEvents = sortEvents $ foldMap toEvents obstacleEdges

      sortEvents = F.toList . sortBy @V.Vector eventCmp . Builder.build @V.Vector

      eventCmp e1 e2 = let a1 = eventPoint e1
                           a2 = eventPoint e2
                       in ccwCmpAroundWith v p a1 a2 <> cmpByDistanceTo  p a1 a2

      initial = Set.fromDistinctAscList . map snd . F.toList
              . sortBy @V.Vector (cmpToRay ray)
              $ mapMaybe (\s -> (,s) <$> ray `intersect` s) obstacleEdges


-- | Given the initial halfine, compare the two intersection points to order the segments.
cmpToRay :: ( LineSegment_ edge point, Point_ point 2 r
            , Ord r, Num r
            , HasSquaredEuclideanDistance edge
            )
         => HalfLine (Point 2 r)
         -> (HalfLineLineSegmentIntersection (Point 2 r) edge, edge)
         -> (HalfLineLineSegmentIntersection (Point 2 r) edge, edge)
         -> Ordering
cmpToRay (HalfLine p _) = comparing (squaredEuclideanDistTo p . fst)


-- | Handle an event in the sweep line algorithm
handle                                    :: ( HasSupportingLine edge
                                             , LineSegment_ edge point
                                             , Point_ point 2 r, Ord r, Num r
                                             , LinePV 2 r `IsIntersectableWith` edge
                                             , Intersection (LinePV 2 r) edge ~
                                               Maybe (LineLineSegmentIntersection edge)
                                             )
                                          => Point 2 r
                                          -> Event r queryPoint edge
                                          -> (Set.Set edge, [queryPoint])
                                          -> (Set.Set edge, [queryPoint])
handle p evt acc@(statusStructure,output) = case evt of
    Query q' q
      | isVisible q' -> (statusStructure, q : output)
      | otherwise    -> acc
    EndPoint q' seg  -> let cmp = cmpSegs (LinePV q' (q' .-. p))
                        in (Set.toggleBy cmp seg statusStructure, output)
    -- note that we are comparing against a line rather than a halfline, since we are
    -- promissed the segments intersect the ray anyway. Furthermore, any segment
    -- can intersect a line only once.
  where
    -- to test if q' is visible we find the closest segment in the status structre
    -- and test if p and q' lie on the same side of this segment.
    isVisible q' = case supportingLine <$> Set.lookupMin statusStructure of
      Nothing -> True
      Just l  -> case onSideTest q' l of
        EQ -> True -- if q' happens to lie *on* this line it is also visible.
        s  -> s == onSideTest p l

--------------------------------------------------------------------------------
-- * Events

data Event r queryPoint seg = Query    !(Point 2 r) queryPoint
                            | EndPoint !(Point 2 r) seg
                            deriving (Show)

-- | Project out the point corresponding to the event
eventPoint :: Event r queryPoint seg -> Point 2 r
eventPoint = \case
  Query    p _ -> p
  EndPoint p _ -> p

-- | Turn a query point into an event
toEvent      :: Point_ queryPoint 2 r => queryPoint -> Builder.Builder (Event r queryPoint seg)
toEvent q    = Builder.singleton $ Query (q^.asPoint) q

-- | Turn a segment into two events (correspoinding to its endpoints)
toEvents      :: (LineSegment_ lineSegment point, Point_ point 2 r)
              => lineSegment -> Builder.Builder (Event r queryPoint lineSegment)
toEvents seg = Builder.singleton (EndPoint (seg^.start.asPoint) seg)
            <> Builder.singleton (EndPoint (seg^.end.asPoint)   seg)

--------------------------------------------------------------------------------
-- * Comparing Segments with respect to a given sweep ray

-- | Compare the the two obstacle edges by their distance along the line to p (the origin
-- of the line.)
cmpSegs                      :: ( LinePV 2 r `IsIntersectableWith` edge
                                , Intersection (LinePV 2 r) edge ~
                                  Maybe (LineLineSegmentIntersection edge)
                                , LineSegment_ edge point
                                , Point_ point 2 r, Ord r, Num r
                                )
                             => LinePV 2 r -- ^ treat the line as a ray actually!
                             -> edge -> edge -> Ordering
cmpSegs l@(LinePV _ _) e1 e2 = cmpIntersection l (f $ l `intersect` e1) (f $ l `intersect` e2)
  where
    f = fromMaybe (error "cmpSegs: precondition failed, no intersection")

-- | Compares the intersection points by distance to p
cmpIntersection                      :: ( LineSegment_ edge point
                                        , Point_ point 2 r, Ord r, Num r
                                        )
                                     => LinePV 2 r
                                     -> LineLineSegmentIntersection edge
                                     -> LineLineSegmentIntersection edge
                                     -> Ordering
cmpIntersection l@(LinePV p _) x1 x2 = cmpByDistanceTo p (f x1) (f x2)
  -- FIXME: what do we do at equality; we should then essentially try again at an epsilon
  -- angle later. (or earlier)
  where
    f = \case
      Line_x_LineSegment_Point q         -> q
      Line_x_LineSegment_LineSegment seg -> min' (seg^.start.asPoint) (seg^.end.asPoint)

    min' a b = case cmpByDistanceTo p a b of
                 GT -> b
                 _  -> a
