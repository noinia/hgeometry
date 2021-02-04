module Algorithms.Geometry.VerticalRayShooting.PersistentSweep where

import           Algorithms.BinarySearch (binarySearchVec)
import           Control.Lens hiding (contains, below)
import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Data.Semigroup.Foldable
import qualified Data.Set as SS -- status struct
import qualified Data.Set.Util as SS
import qualified Data.Vector as V


--------------------------------------------------------------------------------

type StatusStructure p e r = SS.Set (LineSegment 2 p r :+ e)


data VerticalRayShootingStructure p e r =
    VerticalRayShootingStructure { _leftMost    :: r
                                 , _sweepStruct :: V.Vector (r :+ StatusStructure p e r)
                                   -- ^ entry (r :+ s) means that "just" left of "r" the
                                   -- status structure is 's', i.e up to 'r'
                                 }
  deriving (Show,Eq)

-- | x-corodinate of the leftmost endpoint
leftMost :: Getter (VerticalRayShootingStructure p e r) r
leftMost = to _leftMost

sweepStruct  :: Getter (VerticalRayShootingStructure p e r) (V.Vector (r :+ StatusStructure p e r))
sweepStruct = to _sweepStruct

--------------------------------------------------------------------------------
-- * Building the DS


-- | Given a set of \(n\) interiorly pairwise disjoint segments,
-- compute a vertical ray shooting data structure.  (i.e. the
-- endpoints of the segments may coincide).
--
-- pre: no vertical segments (they are ignored)
--
-- running time: \(O(n\log n)\).
-- space: \(O(n\log n)\).
verticalRayShootingStructure   :: (Ord r, Fractional r, Foldable1 t)
                               => t (LineSegment 2 p r :+ e)
                               -> VerticalRayShootingStructure p e r
verticalRayShootingStructure ss = VerticalRayShootingStructure (eventX e) (sweep' events)
  where
    events@(e :| _) = fmap combine
                    . NonEmpty.groupAllWith1 eventX
                    . foldMap1 toEvents
                    $ ss
    sweep' = V.fromList . toList . sweep

combine                    :: NonEmpty (Event p e r) -> Event p e r
combine es@((x :+ _) :| _) = x :+ foldMap1 eventActions es

toEvents   :: Ord r => LineSegment 2 p r :+ e -> NonEmpty (Event p e r)
toEvents s = let (p,q) = bimap (^.core) (^.core) . orderedEndPoints $ s^.core
             in NonEmpty.fromList [ (p^.xCoord) :+ Insert s :| []
                                  , (q^.xCoord) :+ Delete s :| []
                                  ]

----------------------------------------

data Action a = Insert a | Delete a  deriving (Show,Eq)

interpret :: Action a -> (a -> a -> Ordering) -> SS.Set a -> SS.Set a
interpret = \case
  Insert s -> \cmp -> SS.insertBy    cmp s
  Delete s -> \cmp -> SS.deleteAllBy cmp s

type Event p e r = r :+ NonEmpty (Action (LineSegment 2 p r :+ e))

eventX :: Event p e r -> r
eventX = view core

eventActions :: Event p e r -> NonEmpty (Action (LineSegment 2 p r :+ e))
eventActions = view extra


----------------------------------------

sweep    :: (Ord r, Fractional r)
         => NonEmpty (Event p e r) -> NonEmpty (r :+ StatusStructure p e r)
sweep es = NonEmpty.fromList
         . snd . List.mapAccumL h SS.empty
         $ zip (toList es) (NonEmpty.tail es)
  where
    h ss evts = let x :+ ss' = handle ss evts in (ss',x :+ ss')

-- TODO: Verify that mapAccumL does not leak memory like foldl does.

handle                :: (Ord r, Fractional r)
                      => StatusStructure p e r
                      -> (Event p e r, Event p e r)
                      -> r :+ StatusStructure p e r
handle ss ( l :+ acts
          , r :+ _
          )           = let mid = (l+r)/2
                        in r :+ foldr (runActionAt mid) ss acts

runActionAt       :: (Ord r, Fractional r)
                  => r -> Action (LineSegment 2 p r :+ e)
                  -> StatusStructure p e r -> StatusStructure p e r
runActionAt x act = interpret act (ordAt x)


--------------------------------------------------------------------------------
-- * Querying the DS


-- | Find the segment vertically above query point q, if it exists.
--
-- \(O(\log n)\)
segmentAbove :: Ord r
             => Point 2 r -> VerticalRayShootingStructure p e r -> Maybe (LineSegment 2 p r :+ e)
segmentAbove q ds | q^.xCoord < ds^.leftMost = Nothing
                  | otherwise                = binarySearchVec (q `leftOf `) (ds^.sweepStruct)
                                               >>= undefined -- searchInSlab q
  where
    q `leftOf` (r :+ _) = q^.xCoord <= r

-- | Finds the first segment directly above q
searchInSlab      :: Ord r
                  => Point 2 r -> StatusStructure p e r -> Maybe (LineSegment 2 p r :+ e)
searchInSlab q ss = let qs = ClosedLineSegment (q :+ undefined) (q :+ undefined)
                    in undefined -- SS.lookupGT qs ss


  -- mi >>= \i -> vss^?ix i.extra >>= successorOf qy
  -- where
  --   vss   = ds^.sweepStruct
  --   -- the index of the slab containing the query point (if such a slab exists)
  --   mi = binarySearchVec (\(x' :+ _) -> x' > ValB qx) vss
  --   -- the successor in the status-struct corresponding to the current slab
  --   successorOf y ss = let (_,m,r) = SS.splitOn (yCoordAt' (ds^.subdivision) qx) y ss
  --                      in SS.lookupMin $ m <> r


-- -- | General query lifting function
-- queryWith                   :: (r -> r -> Ordering) -> (q -> r) -> (a -> r)
--                             -> (a -> Set a -> b)
--                             -> q -> SS.Set a -> b
-- queryWith cmp f g query q s = withOrd cmp $ liftOrd1 (query (f q)) s














-- -- | Locates the edge (dart) directly above the query point.
-- -- returns Nothing if the query point lies in the outer face and there is no dart
-- -- above it.
-- --
-- -- running time: \(O(\log n)\)
-- dartAbove                   :: (Ord r, Fractional r)
--                             => Point 2 r -> VerticalRayShootingStructure s v e f r -> Maybe (Dart s)
-- dartAbove (Point2 qx qy) ds = mi >>= \i -> vss^?ix i.extra >>= successorOf qy
--   where
--     vss   = ds^.sweepStruct
--     -- the index of the slab containing the query point (if such a slab exists)
--     mi = binarySearchVec (\(x' :+ _) -> x' > ValB qx) vss
--     -- the successor in the status-struct corresponding to the current slab
--     successorOf y ss = let (_,m,r) = SS.splitOn (yCoordAt' (ds^.subdivision) qx) y ss
--                        in SS.lookupMin $ m <> r


-- -- | Locates the face containing the query point.
-- --
-- -- running time: \(O(\log n)\)
-- faceContaining      :: (Ord r, Fractional r)
--                     => Point 2 r -> VerticalRayShootingStructure s v e f r -> FaceId' s
-- faceContaining q ds = maybe (ds^.outerFace) getFace $ dartAbove q ds
--   where
--     ps = ds^.subdivision
--     getFace d = let (u,v) = bimap (^.location) (^.location) $ endPointData d ps
--                 in if u <= v then rightFace d ps
--                              else leftFace  d ps

-- -- type Sweep s v e f r = Reader (PlanarSubdivision s v e f)


-- -- construct   :: PlanarSubdivision s v e f r -> VerticalRayShootingStructure s v e f r
-- -- construct ps = undefined

-- handle                     :: (Ord r, Fractional r)
--                            => PlanarSubdivision s v e f r
--                            -> Bottom r :+ StatusStructure s
--                            -> (VertexId' s, VertexData r v)
--                            -> Bottom r :+ StatusStructure s
-- handle ps (_ :+ ss) (v,vd) = ValB x :+ replace ps (vd^.location) rs ss
--   where
--     x       = vd^.location.xCoord
--     (_,rs)  = partition ps v x . V.toList $ incidentEdges v ps


-- -- | Given the planar subdiv, the location of a vertex v, and a list of edges
-- -- that start at v replaces all edges that end at v by the ones that start at v
-- replace               :: (Ord r, Fractional r)
--                       => PlanarSubdivision s v e f r
--                       -> Point 2 r -> [Dart s]
--                       -> StatusStructure s -> StatusStructure s
-- replace ps p rs ss = below <> SS.fromAscList' rs <> above
--   where
--     (below,_,above) = SS.splitOn (yCoordAt' ps (p^.xCoord)) (p^.yCoord) ss
--     -- note that the 'on' ones are exactly our 'left' segments that we should
--     -- remove

-- -- | Given a vertex v, its x-coordinate, and a list of its incident darts, it
-- -- partitions the darts around x into (ls,rs) where, ls are the darts whose
-- -- other endpoint is left of x and rs are the darts whose other endpoint lies
-- -- right of x.
-- --
-- -- The returned lists are still in clockwise order, ls with the first/topmost edge
-- -- and rs starting with the first/bottommost edge
-- partition           :: Ord r
--                     => PlanarSubdivision s v e f r -> VertexId' s -> r -> [Dart s]
--                     -> ([Dart s], [Dart s])
-- partition ps v x es = case ls of
--                         [] -> (ls' ++ ls'', rs' ++ rs)
--                         _  -> (ls' ++ ls  , rs)
--   where
--     otherEndPointX d = let (u,w) = endPoints d ps
--                            f z   = ps^.locationOf z.xCoord
--                        in if u == v then f w
--                                     else f u
--     (ls,rest)  = L.span (\e -> otherEndPointX e <= x) es
--     (rs,ls')   = L.span (\e -> otherEndPointX e >  x) rest
--     (ls'',rs') = L.span (\e -> otherEndPointX e <= x) ls'

-- --------------------------------------------------------------------------------

type Compare a = a -> a -> Ordering

-- | Compare based on the y-coordinate of the intersection with the horizontal
-- line through y
ordAt   :: (Fractional r, Ord r) => r -> Compare (LineSegment 2 p r :+ e)
ordAt x = comparing (yCoordAt x)


-- | Given an x-coordinate and a line segment that intersects the vertical line
-- through x, compute the y-coordinate of this intersection point.
--
-- note that we will pretend that the line segment is closed, even if it is not
yCoordAt :: (Fractional r, Ord r) => r -> LineSegment 2 p r :+ e -> r
yCoordAt x (LineSegment' (Point2 px py :+ _) (Point2 qx qy :+ _) :+ _)
    | px == qx  = py `max` qy -- s is vertical, since by the precondition it
                              -- intersects we return the y-coord of the topmost
                              -- endpoint.
    | otherwise = py + alpha * (qy - py)
  where
    alpha = (x - px) / (qx - px)

--------------------------------------------------------------------------------
