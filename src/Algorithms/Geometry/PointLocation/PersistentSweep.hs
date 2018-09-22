{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.PointLocation.PersistentSweep where

import           Control.Lens hiding (contains, below)
import           Data.Ext
import           Data.Function (on)
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import qualified Data.List as L
import           Data.Ord (comparing)
import qualified Data.OrdSeq as SS -- status struct
import           Data.Sequence.Util (binarySearchVec)
import           Data.UnBounded
import qualified Data.Vector as V

--------------------------------------------------------------------------------

type StatusStructure s = SS.OrdSeq (Dart s)

data PointLocationDS s v e f r = PointLocationDS {
        _sweepStruct :: V.Vector (Bottom r :+ StatusStructure s)
      , _subdivision :: PlanarSubdivision s v e f r
      , _outerFace   :: FaceId' s
      } deriving (Show,Eq)

sweepStruct :: Getter (PointLocationDS s v e f r) (V.Vector (Bottom r :+ StatusStructure s))
sweepStruct = to _sweepStruct

subdivision :: Getter (PointLocationDS s v e f r) (PlanarSubdivision s v e f r)
subdivision = to _subdivision

outerFace   :: Getter (PointLocationDS s v e f r) (FaceId' s)
outerFace   = to _outerFace


-- | Builds a pointlocation data structure on the planar subdivision with \(n\)
-- vertices.
--
-- running time: \(O(n\log n)\).
-- space: \(O(n\log n)\).
pointLocationDS   :: (Ord r, Fractional r)
                   => PlanarSubdivision s v e f r -> PointLocationDS s v e f r
pointLocationDS ps = PointLocationDS (sweep ps) ps (outerFaceId ps)
  where
    vtxes = L.sortBy (comparing (^._2.location.xCoord)) . V.toList . vertices
    sweep = V.fromList . trim . L.scanl' (handle ps) (Bottom :+ mempty) . vtxes
    -- drop consecutive itmems at the same x-coordinates
    trim  = map L.last . L.groupBy ((==) `on` (^.core))


-- | Locates the edge (dart) directly above the query point.
-- returns Nothing if the query point lies in the outer face and there is no dart
-- above it.
--
-- running time: \(O(\log n)\)
dartAbove                   :: (Ord r, Fractional r)
                            => Point 2 r -> PointLocationDS s v e f r -> Maybe (Dart s)
dartAbove (Point2 qx qy) ds = mi >>= \i -> vss^?ix i.extra >>= successorOf qy
  where
    vss   = ds^.sweepStruct
    -- the index of the slab containing the query point (if such a slab exists)
    mi = binarySearchVec (\(x' :+ _) -> x' > ValB qx) vss
    -- the successor in the status-struct corresponding to the current slab
    successorOf y ss = let (_,m,r) = SS.splitOn (yCoordAt' (ds^.subdivision) qx) y ss
                       in SS.lookupMin $ m <> r


-- | Locates the face containing the query point.
--
-- running time: \(O(\log n)\)
faceContaining      :: (Ord r, Fractional r)
                    => Point 2 r -> PointLocationDS s v e f r -> FaceId' s
faceContaining q ds = maybe (ds^.outerFace) getFace $ dartAbove q ds
  where
    ps = ds^.subdivision
    getFace d = let (u,v) = bimap (^.location) (^.location) $ endPointData d ps
                in if u <= v then rightFace d ps
                             else leftFace  d ps

-- type Sweep s v e f r = Reader (PlanarSubdivision s v e f)


-- construct   :: PlanarSubdivision s v e f r -> PointLocationDS s v e f r
-- construct ps = undefined

handle                     :: (Ord r, Fractional r)
                           => PlanarSubdivision s v e f r
                           -> Bottom r :+ StatusStructure s
                           -> (VertexId' s, VertexData r v)
                           -> Bottom r :+ StatusStructure s
handle ps (_ :+ ss) (v,vd) = (ValB x) :+ replace ps (vd^.location) rs ss
  where
    x       = vd^.location.xCoord
    (_,rs)  = partition ps v x . V.toList $ incidentEdges v ps


-- | Given the planar subdiv, the location of a vertex v, and a list of edges
-- that start at v replaces all edges that end at v by the ones that start at v
replace               :: (Ord r, Fractional r)
                      => PlanarSubdivision s v e f r
                      -> Point 2 r -> [Dart s]
                      -> StatusStructure s -> StatusStructure s
replace ps p rs ss = below <> SS.fromAscList' rs <> above
  where
    (below,_,above) = SS.splitOn (yCoordAt' ps (p^.xCoord)) (p^.yCoord) ss
    -- note that the 'on' ones are exactly our 'left' segments that we should
    -- remove

-- | Given a vertex v, its x-coordinate, and a list of its incident darts, it
-- partitions the darts around x into (ls,rs) where, ls are the darts whose
-- other endpoint is left of x and rs are the darts whose other endpoint lies
-- right of x.
--
-- The returned lists are still in clockwise order, ls with the first/topmost edge
-- and rs starting with the first/bottommost edge
partition           :: Ord r
                    => PlanarSubdivision s v e f r -> VertexId' s -> r -> [Dart s]
                    -> ([Dart s], [Dart s])
partition ps v x es = case ls of
                        [] -> (ls' ++ ls'', rs' ++ rs)
                        _  -> (ls' ++ ls  , rs)
  where
    otherEndPointX d = let (u,w) = endPoints d ps
                           f z   = ps^.locationOf z.xCoord
                       in if u == v then f w
                                    else f u
    (ls,rest)  = L.span (\e -> otherEndPointX e <= x) es
    (rs,ls')   = L.span (\e -> otherEndPointX e >  x) rest
    (ls'',rs') = L.span (\e -> otherEndPointX e <= x) ls'

--------------------------------------------------------------------------------

-- | Compare based on the y-coordinate of the intersection with the horizontal
-- line through y
ordAt   :: (Fractional r, Ord r) => r -> SS.Compare (LineSegment 2 p r)
ordAt x = comparing (yCoordAt x)


-- | Orders two darts by y-coordinate at the given x-coordinate
ordAt'      :: (Fractional r, Ord r) => PlanarSubdivision s v e f r -> r -> SS.Compare (Dart s)
ordAt' ps x = let g d = (edgeSegment d ps)^.core
              in \e f -> ordAt x (g e) (g f)


yCoordAt'        :: (Fractional r, Ord r)
                 => PlanarSubdivision s v e f r
                 -> r -> Dart s -> r
yCoordAt' ps x d = yCoordAt x $ (edgeSegment d ps)^.core

-- | Given an x-coordinate and a line segment that intersects the vertical line
-- through x, compute the y-coordinate of this intersection point.
--
-- note that we will pretend that the line segment is closed, even if it is not
yCoordAt :: (Fractional r, Ord r) => r -> LineSegment 2 p r -> r
yCoordAt x (LineSegment' (Point2 px py :+ _) (Point2 qx qy :+ _))
    | px == qx  = py `max` qy -- s is vertical, since by the precondition it
                              -- intersects we return the y-coord of the topmost
                              -- endpoint.
    | otherwise = py + alpha * (qy - py)
  where
    alpha = (x - px) / (qx - px)

--------------------------------------------------------------------------------
