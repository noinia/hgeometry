--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Triangulation.MakeMonotone
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Polygon.Triangulation.MakeMonotone
  ( computeDiagonals
  , classifyVertices
  , VertexType(..)
  ) where

import           Control.Lens
import           Data.Default.Class
import qualified Data.Map as Map
import           Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import           HGeometry.Ext
import           HGeometry.Foldable.Sort
import           HGeometry.LineSegment
import           HGeometry.Point
import           HGeometry.Polygon.Class
import qualified HGeometry.Set.Util as SS
import           HGeometry.Vector
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder

----------------------------------------------------------------------------------

type Diagonal polygon = Vector 2 (VertexIx polygon)

-- | Given a polygon, find a set of non-intersecting diagonals that partition
-- the polygon into y-monotone pieces.
--
-- running time: \(O(n\log n)\)
computeDiagonals    :: ( Polygon_ polygon point r, Point_ point 2 r
                       , Ord r, Num r, Ord (VertexIx polygon), Default (VertexIx polygon)
                       )
                    => polygon -> [Diagonal polygon]
computeDiagonals pg =
    snd $ Vector.foldl' (handle pg) (StatusStructure Set.empty Map.empty, []) events
    -- sweep line algorithm to compute the diagonals
  where
    events = sortBy compareEvent . Builder.build @Vector.Vector
           $ ifoldMapOf vertices (\i v -> Builder.singleton $ toEvent i v) pg
    -- Transform the vertex into an Event
    toEvent i v = Event (classify pg i v) i v (pg^.ccwPredecessorOf i.withIndex._1)
                                              (pg^.ccwSuccessorOf i.withIndex._1)

-- | Assigns a vertex type to the given vertex
classify        :: ( Polygon_ polygon  point r, Num r, Ord r, Point_ point 2 r)
                => polygon -> VertexIx polygon -> point -> VertexType
classify pg i c = let p = pg^.ccwPredecessorOf i
                      n = pg^.ccwSuccessorOf i
                  in case (p `cmpSweep` c, n `cmpSweep` c, largeInteriorAngle p n) of
                       (LT, LT, False) -> Start
                       (LT, LT, True)  -> Split
                       (GT, GT, False) -> End
                       (GT, GT, True)  -> Merge
                       _               -> Regular
  where
    -- is the angle larger than > 180 degrees
    largeInteriorAngle p n = case ccw p c n of
           CCW -> False
           CW  -> True
           _   -> error "classifyVertices -> largeInteriorAngle: colinear points"

-- | comparison used to order the events w.r.t the sweep line:
-- p < q = p.y < q.y || p.y == q.y && p.x > q.y
cmpSweep       :: (Point_ point 2 r, Ord r) => point -> point -> Ordering
p `cmpSweep` q = comparing (^.yCoord) p q <> comparing (Down . (^.xCoord)) p q


-- | Handle an event
handle :: forall polygon point r. ( Polygon_ polygon point r
                                  , Point_ point 2 r, Num r, Ord r
                                  , Default (VertexIx polygon), Ord (VertexIx polygon)
                                  )
       => polygon
       -> (StatusSruct polygon, [Diagonal polygon]) -> Event polygon
       -> (StatusSruct polygon, [Diagonal polygon])
handle pg (status, diags) (Event et i v p s) = case et of
    Start            -> (insertAt v nextEdge i status, diags)

    End              -> (deleteAt v prevEdge status, diagFromHelper p status <? diags)

    Split            -> ( (insertAt v nextEdge i status)&helpers %~ Map.insert j i
                        , diag : diags)

    Merge            -> ( (deleteAt v prevEdge status)&helpers %~ Map.insert j i
                        , diagFromHelper j status <? diagFromHelper p status <? diags)

    Regular
      | isLeftVertex -> ( insertAt v nextEdge i $ deleteAt v prevEdge status
                        , diagFromHelper p status <? diags )
      | otherwise    -> ( status&helpers %~ Map.insert j i, diagFromHelper j status <? diags)
  where
    -- edges incident to v = v_i
    prevEdge = ClosedLineSegment (pg^?!vertexAt p) v :+ p
    nextEdge = ClosedLineSegment v (pg^?!vertexAt s) :+ i

    -- j is the EdgeId of the edge directly left of v (j may not exist when v is a start,
    -- end, or left-regular vertex; we use it only when we know it exists.)
    j = case lookupLT v status of
          Just (_leftEdge :+ j') -> j'
          _                      -> error "MakeMonotone.handleSplit: absurd. no j"
    -- the diagonal between the current vertex v and the helper
    diag      = case helper j status of
                  Just h -> Vector2 i h
                  _      -> error "MakeMonotone.handleSplit: absurd. no helper"

    -- figure out what the helper of edgeIx is, and if its a merge vertex connect it to
    -- the current vertex v
    diagFromHelper edgeIx status' = case helper edgeIx status' of
                                      Just h | isMerge h -> Just $ Vector2 i h
                                      _                  -> Nothing
      where
        isMerge h = classify pg h (pg^?!vertexAt h) == Merge

    -- | returns True if v the interior of the polygon is to the right of v
    isLeftVertex = case (pg^?!vertexAt p) `cmpSweep` v of
                     GT -> True
                     _  -> False

    -- given a vertex that determines the time, and an edge and its helper, insert it
    -- into the status structure
    insertAt                 :: point
                             -> Edge' polygon
                             -> VertexIx polygon -- ^ the helper of the edge
                             -> StatusSruct polygon -> StatusSruct polygon
    insertAt u seg h status' = status'&tree    %~ SS.insertBy (ordAtY $ u^.yCoord) seg
                                      &helpers %~ Map.insert (seg^.extra) h

    -- given a vertex that determines the dtime, and an edge. Delete the edge from the status
    -- structure.
    deleteAt :: point -> Edge' polygon -> StatusSruct polygon -> StatusSruct polygon
    deleteAt u seg = over tree $ SS.deleteAllBy (ordAtY $ u^.yCoord) seg

    -- Look up the edge directly to the left of the query point q.
    lookupLT      :: point -> StatusSruct polygon -> Maybe (Edge' polygon)
    lookupLT q s' = let (l,_,_) = SS.splitBy (cmpX q) (s'^.tree)
                    in Set.lookupMax l
    -- TODO: verify; before we used lookup LE here

-- | compare the point and the segment horizontally. I.e. returs LT if the point lies left
-- of the segment, EQ if the point is on the segment, and GT otherwise.
cmpX                     :: ( Point_ point  2 r, LineSegment_ lineSegment point'
                            , Point_ point' 2 r, Ord r, Num r
                            ) => point -> lineSegment -> Ordering
cmpX q (orientLR -> seg) = case ccw (seg^.start.asPoint) (q^.asPoint) (seg^.end.asPoint) of
                             CCW      -> LT
                             CoLinear -> EQ
                             CW       -> GT


--------------------------------------------------------------------------------
-- * An Event; essentially just a vertex.

-- | Events during the sweepline algorithm ; the vertices
type Event polygon = Event' (VertexIx polygon) (Vertex polygon)
data Event' idx point =
  Event { _eventType      :: {-# UNPACK #-}!VertexType
        , _vtxId          :: !idx
        , eventPoint      :: !point
        , _predecessorVtx :: !idx
        , _successorVtx   :: !idx
        } deriving (Show,Eq)

-- | how to compare events
compareEvent     :: (Point_ point 2 r, Ord r) => Event' i point -> Event' i point -> Ordering
compareEvent p q = eventPoint q `cmpSweep` eventPoint p
  -- note that the order of p and q has switched.

--------------------------------------------------------------------------------
-- * The Status Structure

-- | edges are identified using their first vertex
type Edge' polygon = ClosedLineSegment (Vertex polygon) :+ VertexIx polygon

-- | The status structure during the sweep
type StatusSruct polygon = StatusStructure (VertexIx polygon)
                                           (Edge' polygon)
                                           (VertexIx polygon)


-- | The status structure consists of a BST of edges intersected by the sweep line (in
-- left to right order), and, for each edge currently intersected by the sweep line its
-- helper vertex.
data StatusStructure edgeId edge vtx =
  StatusStructure { _tree    :: Set.Set edge -- ^ the BST with edges ordered along sweep
                  , _helpers :: Map.Map edgeId vtx -- ^ for each edge its helper
                  } deriving (Show,Eq)

tree    :: Lens' (StatusStructure edgeId edge vtx) (Set.Set edge)
tree    = lens _tree   (\ss t -> ss { _tree   = t })

helpers :: Lens' (StatusStructure edgeId edge vtx) (Map.Map edgeId vtx)
helpers = lens _helpers (\ss h -> ss { _helpers = h })

-- | Get the helper of the given edge
helper          :: Ord edgeId => edgeId -> StatusStructure edgeId edge vtx -> Maybe vtx
helper i status = status^.helpers.at i

--------------------------------------------------------------------------------

-- | The various vertex types
data VertexType = Start | Merge | Split | End | Regular deriving (Show,Read,Eq)

-- | Classify all vertices of the polygon; i.e. compute their vertex types.
classifyVertices    :: ( Polygon_ polygon  point r, Num r, Ord r, Point_ point 2 r)
                    => polygon -> [(VertexIx polygon, VertexType)]
classifyVertices pg = (\(i,v) -> (i, classify pg i v)) <$> pg^@..vertices

--------------------------------------------------------------------------------
-- * Helper functions

infixr 5 <?
-- | Cons the item onto the list (if the item actually exists)
(<?) :: Maybe a -> [a] -> [a]
Nothing  <? xs = xs
(Just x) <? xs = x:xs
