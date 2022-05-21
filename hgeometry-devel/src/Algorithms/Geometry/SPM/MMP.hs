{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.SPM.MMP
  (
  ) where

import           Control.Lens
import           Data.EnumMap.Strict (EnumMap)
import qualified Data.EnumMap.Strict as EnumMap
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Maybe (maybeToList)
import qualified Data.PQueue.Prio.Min as PQueue
import qualified Data.Set as Set
import           Data.UnBounded
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Geometry.LineSegment hiding (endPoints)
import           Geometry.PlanarSubdivision
import           Geometry.Point
import           Witherable

--------------------------------------------------------------------------------


-- ideally we want one world per face,
data MyWorld

data WithDistance s r v = WithDistance { _distanceToSource    :: !r
                                       -- ^ distance to the source
                                       , _predecessorToSource :: !(Maybe (VertexId' s))
                                       -- ^ Nothing if this is the source itself.
                                       , _theVertexData       :: !v
                                       }
                      deriving (Show,Eq)

-- | every vertex is labeled with its distance every fragment contains
-- a planar subdivision htat has the SPM in that fragment.
type ShortestPathMap s v e f r =
  PlanarSubdivision s (WithDistance s r v) e f r

-- | Triangulated planar subdivision.
type Triangles s v e f r =
  PlanarSubdivision s v e f r



-- | Compteu the shortest path map
shortestPathMap :: (Ord r)
                 => Point 2 r -- ^ the source point
                -> PlanarSubdivision s v e f r
                -> ShortestPathMap s v e f r
shortestPathMap = undefined



-- |
shortestPathMap' :: (Ord r)
                 => VertexId' s -- ^ the source point
                 -> Triangles s v e f r --
                 -> ShortestPathMap s v e f r
shortestPathMap' = undefined

--------------------------------------------------------------------------------
-- * Computing the Edge subdivision


-- | A generator gives rise to a candidate interval on an edge.
data Generator s r = Generator { _root         :: !(VertexId' s)
                               , _initialDist  :: !r
                               , _unfoldedRoot :: !(Point 2 r)
                               -- ^ point corresponding to the root in the same coordinate
                               -- system as the face opposite to the edge this generator
                               -- corresponds to.
                               --
                               -- note that in the plane the unfolded root is just the same
                               -- as the original location of the root.
                               } deriving (Show)

newtype EdgeSubdivision s r =
    EdgeSubdivision (Set.Set (Generator s r))
  deriving (Show)






data EdgeSPM s r = EdgeSPM { _vertexDistances  :: EnumMap (VertexId' s) (Top r)
                           , _edgeSubdivisions :: EnumMap (Dart s)      (EdgeSubdivision s r)
                           }
                 deriving (Show)

permanentlyLabel                              :: VertexId' s -> Top r -> EdgeSPM s r
                                              -> EdgeSPM s r
permanentlyLabel v dist (EdgeSPM vds subdivs) = EdgeSPM (EnumMap.insert v dist vds) subdivs


-- | Shortest path map decomposition of the edges
edgeShortestPathMap :: (Ord r, Num r)
                     => VertexId' s -- ^ the source
                    -> Triangles s v e f r
                    -> PlanarSubdivision s (WithDistance s r v)
                                           (e, EdgeSubdivision s r)
                                           f
                                           r
edgeShortestPathMap = undefined



-- | for every portal edge the subidision up to f-free paths
edgeShortestPathMapPortalEdge :: (Ord r, Num r)
                              => VertexId' s
                              -> Triangles s v e f r
                              -> EdgeSPM s r
edgeShortestPathMapPortalEdge s subdiv = computeIntervals s subdiv queue
                                       $ EdgeSPM initialDistances initialSubdivs
  where
    -- initialize all vertex distances on +Infty and s on 0
    initialDistances = EnumMap.insert s (ValT 0)
                     . EnumMap.fromAscList . map (\v -> (v,Top)) . F.toList
                     $ vertices' subdiv

    -- all subdivisions are empty, except for the edges opposite to s
    initialSubdivs  = insertOppositeEdges
                    . EnumMap.fromAscList . map (\e -> (e,EdgeSubdivision Set.empty)) . F.toList
                    $ darts' subdiv

    -- for each edge opposite to s, insert an interval with s as generator
    insertOppositeEdges eSubdivs0 = foldr insertEdge eSubdivs0 oppositeEdges
    insertEdge = undefined

    -- the edges opposite to s
    oppositeEdges = edgesOppositeTo s subdiv

    -- initialize the eventQueue
    queue = foldr (uncurry PQueue.insert) mempty initialEvents

    -- for each edge oppisite to s we add the appropriate events
    -- (endpoints and closest interior point)
    initialEvents = flip concatMap oppositeEdges $ \e ->
      let (u,v) = endPoints e subdiv
          f w   = ( distance (subdiv^.locationOf w) locS
                  , Event $ VertexEvent w
                  )
          mEvt = (\(c,d) -> (d,Event $ InteriorEvent e gen0 c))
                 <$> closestInteriorPointOn (edgeSegment e subdiv) locS
      in maybeToList mEvt <> [ f u, f v ]

    locS = subdiv^.locationOf s
    gen0 = Generator s 0 locS


closestInteriorPointOn            :: LineSegment 2 p r :+ e -> Point 2 r
                                  -> Maybe (Point 2 r, r)
closestInteriorPointOn (s :+ _) q =
  undefined --   sqrt' <$> pointClosestToWithDistance q s
  -- todo get rid of the point if it is and endpoint; otherwise we add that event twice.

sqrt' = undefined

distance :: Point 2 r -> Point 2 r -> r
distance = undefined


-- | Get all edges opposite to s
--
-- TODO: Test if this should indeed be prevIncidentEdge
edgesOppositeTo          :: VertexId' s -> PlanarSubdivision s v e f r -> Vector (Dart s)
edgesOppositeTo s subdiv = flip prevIncidentEdge subdiv <$> outgoingEdges s subdiv



type EventQueue s r = PQueue.MinPQueue r (Event s r)


data Event s r = Event { eventKind :: EventKind s r }
  deriving (Show)



-- (Point 2 r) EventKind

data EventKind s r = VertexEvent (VertexId' s)
                   | InteriorEvent (Dart s) (Generator s r) (Point 2 r)
                   deriving (Show)


computeIntervals          :: Ord r
                          => VertexId' s
                          -> Triangles s v e f r
                          -> EventQueue s r
                          -> EdgeSPM s r
                          -> EdgeSPM s r
computeIntervals s subdiv = go
  where
    go queue state = case PQueue.minViewWithKey queue of
      Nothing           -> state -- done
      Just (evt,queue') -> let (newEvts, state') = propagate subdiv state evt
                           in go (insertAll newEvts queue') state'

    insertAll = undefined -- foldr PQueue.insert


-- | Propagate an interval
propagate      :: Triangles s v e f r
               -> EdgeSPM s r -> (r, Event s r ) -> ( [Event s r] -- new events
                                                    , EdgeSPM s r
                                                    )
propagate subdiv state (dist,evt) = case eventKind evt of
    InteriorEvent e gen c -> undefined
    VertexEvent v         -> let es = mapMaybe (createCandidateInterval
                                                (Generator v dist (subdiv^.locationOf v))
                                                undefined -- the access point
                                                -- TODO: figure out if this should just be
                                               )
                                    $ edgesOppositeTo v subdiv
                                      -- todo insert the intervals es onto the appropriate edges
                             in ( undefined
                                , permanentlyLabel v (ValT dist) $ state
                                )






-- | creates an interval on e with generator gen if applicable

-- TODO this still misses the access point
createCandidateInterval       :: Generator s r
                              -> Point 2 r -- access point
                              -> Dart s -> Maybe (Generator s r)
createCandidateInterval gen beta e =  undefined


--------------------------------------------------------------------------------
-- * Filling in the faces goes here
