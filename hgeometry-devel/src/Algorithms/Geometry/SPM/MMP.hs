{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.SPM.MMP
  ( ShortestPathMap
  , edgeShortestPathMap

  ) where

import           Control.Lens
import           Data.EnumMap.Strict (EnumMap)
import qualified Data.EnumMap.Strict as EnumMap
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Maybe (maybeToList, fromMaybe)
import qualified Data.PQueue.Prio.Min as PQueue
import           Data.Range
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




--------------------------------------------------------------------------------




--------------------------------------------------------------------------------


newtype EdgeSubdivision s r =
    EdgeSubdivision (Set.Set (SPMInterval s r))
  deriving (Show)


data InsertResult s r = InsertResult { _deleted      :: [SPMInterval s r]
                                     , _trimmedLeft  :: Maybe (Point 2 r)
                                     , _trimmedRight :: Maybe (Point 2 r)
                                     , _leftNeigh    :: Maybe (SPMInterval s r)
                                     , _rightNeigh   :: Maybe (SPMInterval s r)
                                     , _newSubdiv    :: EdgeSubdivision s r
                                     }
                        deriving (Show)

makeLenses ''InsertResult





insertInterval' :: Dart s -> SPMInterval s r -> EdgeSubdivision s r -> EdgeSubdivision s r
insertInterval' = undefined

insert                 :: Point 2 r -> SPMInterval s r
                       -> Dart s -> EdgeSubdivision s r -> InsertResult s r
insert c i e intervals = undefined



splitLocate             :: Point 2 r -> EdgeSubdivision s r
                        -> (EdgeSubdivision s r,Maybe (SPMInterval s r), EdgeSubdivision s r)
splitLocate c intervals = undefined




--------------------------------------------------------------------------------

-- | every vertex is labeled with its distance every fragment contains
-- a planar subdivision htat has the SPM in that fragment.
type ShortestPathMap s v e f r =
  PlanarSubdivision s (v, Top (WithDistance s r)) e f r

-- | Triangulated planar subdivision.
type Triangles s v e f r =
  PlanarSubdivision s v e f r



-- | Compute the shortest path map of a given source point
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

-- | Shortest path map decomposition of the edges
edgeShortestPathMap :: (Ord r, Num r)
                     => VertexId' s -- ^ the source
                    -> Triangles s v e f r
                    -> PlanarSubdivision s (v, Top (WithDistance s r))
                                           (e, EdgeSubdivision s r)
                                           f r
edgeShortestPathMap s subdiv = mergeAllIntervals
                             . mapDarts    (\d e  -> (e, edgeSubdivOf d edgeSPM))
                             . mapVertices (\vi v -> (v, distanceOf vi edgeSPM))
                             $ subdiv
  where
    edgeSPM = edgeShortestPathMapPortalEdge s subdiv

mergeAllIntervals subdiv = subdiv
  -- mapEdges (\d (e,es) -> let es' = subdiv^.dataOf (twin d)._2
  --                                                 in (e, mergeIntervals es es')
  --                                   )
  -- FIXME: implement this properly


-- | merge the two edge subdivisions so that we have the actual shortest paths
mergeIntervals :: EdgeSubdivision s r -> EdgeSubdivision s r -> EdgeSubdivision s r
mergeIntervals = undefined


-- | for every portal edge the subidision up to f-free paths
edgeShortestPathMapPortalEdge :: (Ord r, Num r)
                              => VertexId' s
                              -> Triangles s v e f r
                              -> SimState s r
edgeShortestPathMapPortalEdge s subdiv = computeIntervals subdiv
                                       $ (queue, SimState initialDistances initialSubdivs)
  where
    -- initialize all vertex distances on +Infty and s on 0
    initialDistances = EnumMap.insert s (ValT $ WithDistance 0 Nothing)
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
          f w   = ( WithDistance (distance (subdiv^.locationOf w) locS) (Just s)
                  , Event $ VertexEvent w
                  )
          mEvt = (\(c,d) -> ( WithDistance d (Just s)
                            , Event $ InteriorEvent e (spmInterval u v) c)
                 )
                 <$> closestInteriorPointOn (edgeSegment e subdiv) locS
      in maybeToList mEvt <> [ f u, f v ]

    locS = subdiv^.locationOf s
    gen0 = Generator s (WithDistance 0 Nothing) locS

    spmInterval u v = SPMInterval gen0
                                  (ClosedRange (subdiv^.locationOf u) (subdiv^.locationOf v))


----------------------------------------







----------------------------------------


data SimState s r =
  SimState { _vertexDistances  :: EnumMap (VertexId' s) (Top (WithDistance s r))
          , _edgeSubdivisions :: EnumMap (Dart s)      (EdgeSubdivision s r)
          }
                 deriving (Show)

-- | Get the current distance estimate and predecessor to v
distanceOf                  :: VertexId' s -> SimState s r -> Top (WithDistance s r)
distanceOf v (SimState vd _) = fromMaybe Top $ EnumMap.lookup v vd

-- | get the edge subdivision of a given dart
edgeSubdivOf                  :: Dart s -> SimState s r -> EdgeSubdivision s r
edgeSubdivOf d (SimState _ es) = fromMaybe err $ EnumMap.lookup d es
  where
    err = error $ "edgeSubdivOf: absurd, dart " <> show d <> " not found!?"


-- | Permanently assign a distance label to a vertex
permanentlyLabel                              :: VertexId' s
                                              -> Top (WithDistance s r)
                                              -> SimState s r -> SimState s r
permanentlyLabel v dist (SimState vds subdivs) = SimState (EnumMap.insert v dist vds) subdivs


insertSPMInterval                     :: Dart s -> SPMInterval s r -> SimState s r -> SimState s r
insertSPMInterval e i (SimState vs es) = SimState vs (EnumMap.adjust (insertInterval' e i) e es)


-- | apply the given function on the edge subdivision of the given edge
apply                     :: Functor f
                          => (EdgeSubdivision s r -> f (EdgeSubdivision s r))
                          -> Dart s -> SimState s r -> f (SimState s r)
apply f d (SimState vd es) = case EnumMap.lookup d es of
    Nothing   -> error $ "apply: absurd, dart " <> show d <> " not found!?"
    Just ints -> SimState vd . (\ints' -> EnumMap.insert d ints' es) <$> f ints

--------------------------------------------------------------------------------


closestInteriorPointOn            :: LineSegment 2 p r :+ e -> Point 2 r
                                  -> Maybe (Point 2 r, r)
closestInteriorPointOn (s :+ _) q =
  undefined --   sqrt' <$> pointClosestToWithDistance q s
  -- todo get rid of the point if it is and endpoint; otherwise we add that event twice.



-- | Get all edges opposite to s
--
-- TODO: Test if this should indeed be prevIncidentEdge
edgesOppositeTo          :: VertexId' s -> PlanarSubdivision s v e f r -> Vector (Dart s)
edgesOppositeTo s subdiv = flip prevIncidentEdge subdiv <$> outgoingEdges s subdiv



type EventQueue s r = PQueue.MinPQueue (WithDistance s r) (Event s r)
-- TODO seems like we need some sort of PSQueue to support deletions


data Event s r = Event { eventKind :: EventKind s r }
  deriving (Show)



-- (Point 2 r) EventKind

data EventKind s r = VertexEvent (VertexId' s)
                   | InteriorEvent (Dart s) (SPMInterval s r) (Point 2 r)
                   deriving (Show)

--------------------------------------------------------------------------------

computeIntervals          :: Ord r
                          => Triangles s v e f r
                          -> (EventQueue s r, SimState s r)
                          -> SimState s r
computeIntervals subdiv = go
  where
    go (queue, state) = case PQueue.minViewWithKey queue of
      Nothing           -> state -- done
      Just (evt,queue') -> go $ propagate subdiv (queue, state) evt

-- | Propagate an interval
propagate                               :: forall s v e f r.
                                           Triangles s v e f r
                                        -> (EventQueue s r, SimState s r)
                                        -> ( WithDistance s r
                                           , Event s r
                                           )
                                        -> ( EventQueue s r
                                           , SimState s r
                                           )
propagate subdiv z0 (dist,evt) = foldr doInsert z0 newIntervals
  where
    doInsert (e,int,c) (queue,spmEdges) = apply (doInsertInterval int c queue e) e spmEdges

    newIntervals :: [(Dart s, SPMInterval s r, Point 2 r)]
    newIntervals = case eventKind evt of
      InteriorEvent e int c -> undefined -- propagateInterior subdiv state e int c
      VertexEvent v         -> undefined -- propagateVertex   subdiv state dist v

propagateInterior subdiv state e int c = ( undefined
                                         , undefined
                                         )
  where
    -- the face on the other side of the dart e; i.e. the face we are propatating onto
    theFace = rightFace e
    e1 = prevIncidentEdge (twin e) subdiv
    e2 = nextIncidentEdge (twin e) subdiv


--------------------------------------------------------------------------------

doInsertInterval                       :: SPMInterval s r -- ^ the interval to insert
                                       -> Point 2 r       -- ^ the frontierpoint of the interval
                                       -> EventQueue s r
                                       -> Dart s -- ^ the edge in question
                                       -> EdgeSubdivision s r -- ^ subdivision of e
                                       -> (EventQueue s r, EdgeSubdivision s r)
doInsertInterval i c queue e intervals = (queue', intervals')
  where
    queue' = undefined
    intervals' = undefined


--------------------------------------------------------------------------------

project         :: SPMInterval s r -> LineSegment 2 p r -> SPMInterval s r
project int seg = int&edgeRange %~ extend
  where
    gen' = int^.generator
    extend (Range l r) = undefined
      -- match (lineThrough (gen'^.unfoldedRoot) l `intersect` seg ) $
      -- H \NoIntersection ->



-- data Cone p r = Cone { _apex           :: !(Point 2 r :+ p)
--                      , _leftDirection  :: !(Vector 2 r)
--                      , _rightDirection :: !(Vector 2 r)
--                      } deriving (Show,Eq)
-- type instance NumType (Cone p r) = r
-- type instance Dimension (Cone p r) = 2

-- trimToCone :: LineSegment 2 p r -> Cone r -> LineSegment 2 p r
-- trimToCone = undefined


propagateVertex                     :: ()
                                    => Triangles s v e f r
                                    -> SimState s r
                                    -> WithDistance s r
                                    -> VertexId' s
                                    -> ( [Event s r] -- new events
                                       , SimState s r
                                       )
propagateVertex subdiv state dist v =
    ( undefined
    , permanentlyLabel v (ValT dist) $ state
         -- todo insert the intervals es onto the appropriate edges
    )
  where
    p  =  undefined -- p should be the predecessor, i.e. where we came from
    es = mapMaybe (createCandidateInterval
                    (Generator v dist (subdiv^.locationOf v))
                    undefined -- the access point
                    -- TODO: figure out if this should just be
                  )
         $ edgesOppositeTo v subdiv




-- | creates an interval on e with generator gen if applicable

-- TODO this still misses the access point
createCandidateInterval       :: Generator s r
                              -> Point 2 r -- access point
                              -> Dart s -> Maybe (Generator s r)
createCandidateInterval gen beta e =  undefined


--------------------------------------------------------------------------------
-- * Filling in the faces goes here
