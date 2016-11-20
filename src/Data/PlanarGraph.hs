{-# LANGUAGE TemplateHaskell #-}
module Data.PlanarGraph( Arc(..)
                       , Direction(..), rev

                       , Dart(..), arc, direction
                       , twin, isPositive

                       , World(..)

                       , Dual

                       , VertexId(..)

                       , PlanarGraph
                       , embedding, vertexData, dartData, faceData
                       , edgeData

                       , planarGraph, planarGraph', fromAdjacencyLists

                       , numVertices, numDarts, numEdges, numFaces
                       , darts', darts, edges', edges, vertices', vertices, faces', faces

                       , tailOf, headOf, endPoints
                       , incidentEdges, incomingEdges, outgoingEdges, neighboursOf

                       , vDataOf, eDataOf, fDataOf, endPointDataOf, endPointData


                       , dual

                       , FaceId(..)
                       , leftFace, rightFace, boundary, boundaryVertices
                       ) where

import           Control.Lens
import           Control.Monad (forM_)
import qualified Data.CircularList as C
import qualified Data.Foldable as F
import qualified Data.Map.Strict as SM
import           Data.Permutation
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- $setup
-- >>> :{
-- let dart i s = Dart (Arc i) (read s)
--     (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]
--     myGraph :: PlanarGraph Test Primal_ () String ()
--     myGraph = planarGraph [ [ (Dart aA Negative, "a-")
--                             , (Dart aC Positive, "c+")
--                             , (Dart aB Positive, "b+")
--                             , (Dart aA Positive, "a+")
--                             ]
--                           , [ (Dart aE Negative, "e-")
--                             , (Dart aB Negative, "b-")
--                             , (Dart aD Negative, "d-")
--                             , (Dart aG Positive, "g+")
--                             ]
--                           , [ (Dart aE Positive, "e+")
--                             , (Dart aD Positive, "d+")
--                             , (Dart aC Negative, "c-")
--                             ]
--                           , [ (Dart aG Negative, "g-")
--                             ]
--                           ]
-- :}

-- TODO: Add a fig. of the Graph


--------------------------------------------------------------------------------

-- | An Arc is a directed edge in a planar graph. The type s is used to tie
-- this arc to a particular graph.
newtype Arc s = Arc { _unArc :: Int } deriving (Eq,Ord,Enum,Bounded)

instance Show (Arc s) where
  show (Arc i) = "Arc " ++ show i


data Direction = Negative | Positive deriving (Eq,Ord,Bounded,Enum)

instance Show Direction where
  show Positive = "+1"
  show Negative = "-1"

instance Read Direction where
  readsPrec _ "-1" = [(Negative,"")]
  readsPrec _ "+1" = [(Positive,"")]
  readsPrec _ _    = []

-- | Reverse the direcion
rev          :: Direction -> Direction
rev Negative = Positive
rev Positive = Negative

-- | A dart represents a bi-directed edge. I.e. a dart has a direction, however
-- the dart of the oposite direction is always present in the planar graph as
-- well.
data Dart s = Dart { _arc       :: !(Arc s)
                   , _direction :: !Direction
                   } deriving (Eq,Ord)
makeLenses ''Dart



instance Show (Dart s) where
  show (Dart a d) = "Dart (" ++ show a ++ ") " ++ show d

-- | Get the twin of this dart (edge)
--
-- >>> twin (dart 0 "+1")
-- Dart (Arc 0) -1
-- >>> twin (dart 0 "-1")
-- Dart (Arc 0) +1
twin            :: Dart s -> Dart s
twin (Dart a d) = Dart a (rev d)

-- | test if a dart is Positive
isPositive   :: Dart s -> Bool
isPositive d = d^.direction == Positive


instance Enum (Dart s) where
  toEnum x
    | even x    = Dart (Arc $ x `div` 2)       Positive
    | otherwise = Dart (Arc $ (x `div` 2) + 1) Negative
  -- get the back edge by adding one

  fromEnum (Dart (Arc i) d) = case d of
                                Positive -> 2*i
                                Negative -> 2*i + 1


-- | The world in which the graph lives
data World = Primal_ | Dual_ deriving (Show,Eq)

type family Dual (sp :: World) where
  Dual Primal_ = Dual_
  Dual Dual_   = Primal_


-- | A vertex in a planar graph. A vertex is tied to a particular planar graph
-- by the phantom type s, and to a particular world w.
newtype VertexId s (w :: World) = VertexId { _unVertexId :: Int } deriving (Eq,Ord,Enum)
-- VertexId's are in the range 0...|orbits|-1

instance Show (VertexId s w) where
  show (VertexId i) = "VertexId " ++ show i


--------------------------------------------------------------------------------
-- * The graph type itself

-- | A *connected* Planar graph with bidirected edges. I.e. the edges (darts) are
-- directed, however, for every directed edge, the edge in the oposite
-- direction is also in the graph.
--
-- The types v, e, and f are the are the types of the data associated with the
-- vertices, edges, and faces, respectively.
--
-- The orbits in the embedding are assumed to be in counterclockwise order.
data PlanarGraph s (w :: World) v e f = PlanarGraph { _embedding  :: Permutation (Dart s)
                                                    , _vertexData :: V.Vector v
                                                    , _rawDartData :: V.Vector e
                                                    , _faceData    :: V.Vector f
                                                    }
                                      deriving (Show,Eq)
makeLenses ''PlanarGraph


-- | lens to access the Dart Data
dartData :: Lens (PlanarGraph s w v e f) (PlanarGraph s w v e' f)
                 (V.Vector (Dart s, e)) (V.Vector (Dart s, e'))
dartData = lens darts (\g xs -> g&rawDartData .~ reorderEdgeData xs)

-- | edgeData is just an alias for 'dartData'
edgeData :: Lens (PlanarGraph s w v e f) (PlanarGraph s w v e' f)
                 (V.Vector (Dart s, e)) (V.Vector (Dart s, e'))
edgeData = dartData


-- | Reorders the edge data to be in the right order to set edgeData
reorderEdgeData    :: Foldable f => f (Dart s, e) -> V.Vector e
reorderEdgeData ds = V.create $ do
                                  v <- MV.new (F.length ds)
                                  forM_ (F.toList ds) $ \(d,x) ->
                                    MV.write v (fromEnum d) x
                                  pure v


-- | Construct a planar graph
planarGraph'      :: Permutation (Dart s) -> PlanarGraph s Primal_ () () ()
planarGraph' perm = PlanarGraph perm vData eData fData
  where
    d = size perm
    e = d `div` 2
    v = V.length (perm^.orbits)
    f = e - v + 2

    vData  = V.replicate v ()
    eData  = V.replicate d ()
    fData  = V.replicate f ()



-- | Construct a planar graph, given the darts in cyclic order around each
-- vertex.
--
-- running time: $O(n)$.
planarGraph    :: [[(Dart s,e)]] -> PlanarGraph s Primal_ () e ()
planarGraph ds = (planarGraph' perm)&dartData .~ (V.fromList . concat $ ds)
  where
    n     = sum . map length $ ds
    perm  = toCycleRep n $ map (map fst) ds


-- - m: a Map, mapping edges, represented by a pair of vertexId's (u,v) with
--            u < v, to arcId's.
-- - a: the next available unused arcID
-- - x: the data value we are interested in computing
type STR' s b = STR (SM.Map (VertexId s Primal_,VertexId s Primal_) Int) Int b


-- | Construct a planar graph from a adjacency matrix. For every vertex, all
-- vertices should be given in counter clockwise order.
--
-- running time: $O(n \log n)$.
fromAdjacencyLists      :: forall s.
                           [(VertexId s Primal_, C.CList (VertexId s Primal_))]
                        -> PlanarGraph s Primal_ () () ()
fromAdjacencyLists adjM = planarGraph' . toCycleRep n $ perm
  where
    n    = sum . fmap length $ adjM
    perm = trd' . foldr toOrbit (STR mempty 0 mempty) $ adjM


    -- | Given a vertex with its adjacent vertices (u,vs) (in CCW order) convert this
    -- vertex with its adjacent vertices into an Orbit
    toOrbit                     :: (VertexId s Primal_, C.CList (VertexId s Primal_))
                                -> STR' s [[Dart s]]
                                -> STR' s [[Dart s]]
    toOrbit (u,vs) (STR m a dss) =
      let (STR m' a' ds') = foldr (toDart . (u,)) (STR m a mempty) . C.toList $ vs
      in STR m' a' (ds':dss)


    -- | Given an edge (u,v) and a triplet (m,a,ds) we construct a new dart
    -- representing this edge.
    toDart                    :: (VertexId s Primal_,VertexId s Primal_)
                              -> STR' s [Dart s]
                              -> STR' s [Dart s]
    toDart (u,v) (STR m a ds) = let dir = if u < v then Positive else Negative
                                    t'  = (min u v, max u v)
                               in case SM.lookup t' m of
      Just a' -> STR m                  a     (Dart (Arc a') dir : ds)
      Nothing -> STR (SM.insert t' a m) (a+1) (Dart (Arc a)  dir : ds)


--------------------------------------------------------------------------------

-- | Get the number of vertices
--
-- >>> numVertices myGraph
-- 4
numVertices :: PlanarGraph s w v e f -> Int
numVertices g = V.length (g^.embedding.orbits)

-- | Get the number of Darts
--
-- >>> numDarts myGraph
-- 12
numDarts :: PlanarGraph s w v e f -> Int
numDarts g = size (g^.embedding)

-- | Get the number of Edges
--
-- >>> numEdges myGraph
-- 6
numEdges :: PlanarGraph s w v e f -> Int
numEdges g = numDarts g `div` 2

-- | Get the number of faces
--
-- >>> numFaces myGraph
-- 4
numFaces   :: PlanarGraph s w v e f -> Int
numFaces g = numEdges g - numVertices g + 2


-- | Enumerate all vertices
--
-- >>> vertices' myGraph
-- [VertexId 0,VertexId 1,VertexId 2,VertexId 3]
vertices'   :: PlanarGraph s w v e f -> V.Vector (VertexId s w)
vertices' g = VertexId <$> V.enumFromN 0 (V.length (g^.embedding.orbits))

-- | Enumerate all vertices, together with their vertex data

-- >>> vertices myGraph
-- [(VertexId 0,()),(VertexId 1,()),(VertexId 2,()),(VertexId 3,())]
vertices   :: PlanarGraph s w v e f -> V.Vector (VertexId s w, v)
vertices g = V.zip (vertices' g) (g^.vertexData)


-- | Enumerate all darts
darts' :: PlanarGraph s w v e f -> V.Vector (Dart s)
darts' = elems . _embedding

-- | Get all darts together with their data
--
-- >>> mapM_ print $ darts myGraph
-- (Dart (Arc 0) -1,"a-")
-- (Dart (Arc 2) +1,"c+")
-- (Dart (Arc 1) +1,"b+")
-- (Dart (Arc 0) +1,"a+")
-- (Dart (Arc 4) -1,"e-")
-- (Dart (Arc 1) -1,"b-")
-- (Dart (Arc 3) -1,"d-")
-- (Dart (Arc 5) +1,"g+")
-- (Dart (Arc 4) +1,"e+")
-- (Dart (Arc 3) +1,"d+")
-- (Dart (Arc 2) -1,"c-")
-- (Dart (Arc 5) -1,"g-")
darts   :: PlanarGraph s w v e f -> V.Vector (Dart s, e)
darts g = (\d -> (d,g^.eDataOf d)) <$> darts' g

-- | Enumerate all edges. We report only the Positive darts
edges' :: PlanarGraph s w v e f -> V.Vector (Dart s)
edges' = V.filter isPositive . darts'

-- | Enumerate all edges with their edge data. We report only the Positive
-- darts.
--
-- >>> mapM_ print $ edges myGraph
-- (Dart (Arc 2) +1,"c+")
-- (Dart (Arc 1) +1,"b+")
-- (Dart (Arc 0) +1,"a+")
-- (Dart (Arc 5) +1,"g+")
-- (Dart (Arc 4) +1,"e+")
-- (Dart (Arc 3) +1,"d+")
edges :: PlanarGraph s w v e f -> V.Vector (Dart s, e)
edges = V.filter (isPositive . fst) . darts






-- | The tail of a dart, i.e. the vertex this dart is leaving from
--
-- running time: $O(1)$
tailOf     :: Dart s -> PlanarGraph s w v e f -> VertexId s w
tailOf d g = VertexId . fst $ lookupIdx (g^.embedding) d

-- | The vertex this dart is heading in to
--
-- running time: $O(1)$
headOf   :: Dart s -> PlanarGraph s w v e f -> VertexId s w
headOf d = tailOf (twin d)

-- | endPoints d g = (tailOf d g, headOf d g)
--
-- running time: $O(1)$
endPoints :: Dart s -> PlanarGraph s w v e f -> (VertexId s w, VertexId s w)
endPoints d g = (tailOf d g, headOf d g)


-- | All edges incident to vertex v, in counterclockwise order around v.
--
-- running time: $O(k)$, where $k$ is the output size
incidentEdges                :: VertexId s w -> PlanarGraph s w v e f
                             -> V.Vector (Dart s)
incidentEdges (VertexId v) g = g^.embedding.orbits.ix' v

-- | All incoming edges incident to vertex v, in counterclockwise order around v.
incomingEdges     :: VertexId s w -> PlanarGraph s w v e f -> V.Vector (Dart s)
incomingEdges v g = V.filter (not . isPositive) $ incidentEdges v g

-- | All outgoing edges incident to vertex v, in counterclockwise order around v.
outgoingEdges     :: VertexId s w -> PlanarGraph s w v e f -> V.Vector (Dart s)
outgoingEdges v g = V.filter isPositive $ incidentEdges v g


-- | Gets the neighbours of a particular vertex, in counterclockwise order
-- around the vertex.
--
-- running time: $O(k)$, where $k$ is the output size
neighboursOf     :: VertexId s w -> PlanarGraph s w v e f -> V.Vector (VertexId s w)
neighboursOf v g = otherVtx <$> incidentEdges v g
  where
    otherVtx d = let u = tailOf d g in if u == v then headOf d g else u

-- outgoingNeighbours :: VertexId s w -> PlanarGraph s w v e f -> V.Vector (VertexId s w)
-- outgoingNeighbours = undefined

-- incomingNeighbours :: VertexId s w -> PlanarGraph s w v e f -> V.Vector (VertexId s w)
-- incomingNeighbours = undefined


--------------------------------------------------------------------------------
-- * Access data

-- | Get the vertex data associated with a node. Note that updating this data may be
-- expensive!!
--
-- running time: $O(1)$
vDataOf              :: VertexId s w -> Lens' (PlanarGraph s w v e f) v
vDataOf (VertexId i) = vertexData.ix' i

-- | Edge data of a given dart
--
-- running time: $O(1)$
eDataOf   :: Dart s -> Lens' (PlanarGraph s w v e f) e
eDataOf d = rawDartData.ix' (fromEnum d)

-- | Data of a face of a given face
--
-- running time: $O(1)$
fDataOf                       :: FaceId s w -> Lens' (PlanarGraph s w v e f) f
fDataOf (FaceId (VertexId i)) = faceData.ix' i


-- | Data corresponding to the endpoints of the dart
endPointDataOf   :: Dart s -> Getter (PlanarGraph s w v e f) (v,v)
endPointDataOf d = to $ endPointData d


-- | Data corresponding to the endpoints of the dart
--
-- running time: $O(1)$
endPointData     :: Dart s -> PlanarGraph s w v e f -> (v,v)
endPointData d g = let (u,v) = endPoints d g in (g^.vDataOf u, g^.vDataOf v)

--------------------------------------------------------------------------------
-- * The Dual graph

-- | The dual of this graph
--
-- >>> :{
--  let fromList = V.fromList
--      answer = fromList [ fromList [dart 0 "-1"]
--                        , fromList [dart 2 "+1",dart 4 "+1",dart 1 "-1",dart 0 "+1"]
--                        , fromList [dart 1 "+1",dart 3 "-1",dart 2 "-1"]
--                        , fromList [dart 4 "-1",dart 3 "+1",dart 5 "+1",dart 5 "-1"]
--                        ]
--  in (dual myGraph)^.embedding.orbits == answer
-- :}
-- True
--
-- running time: $O(n)$.
dual   :: PlanarGraph s w v e f -> PlanarGraph s (Dual w) f e v
dual g = let perm = g^.embedding
         in PlanarGraph (cycleRep (elems perm) (apply perm . twin))
                        (g^.faceData)
                        (g^.rawDartData)
                        (g^.vertexData)

-- | A face
newtype FaceId s w = FaceId { _unFaceId :: VertexId s (Dual w) } deriving (Eq,Ord)

instance Show (FaceId s w) where
  show (FaceId (VertexId i)) = "FaceId " ++ show i

-- | Enumerate all faces in the planar graph
faces' :: PlanarGraph s w v e f -> V.Vector (FaceId s w)
faces' = fmap FaceId . vertices' . dual

-- | All faces with their face data.
faces   :: PlanarGraph s w v e f -> V.Vector (FaceId s w, f)
faces g = V.zip (faces' g) (g^.faceData)

-- | The face to the left of the dart
--
-- >>> leftFace (dart 1 "+1") myGraph
-- FaceId 1
-- >>> leftFace (dart 1 "-1") myGraph
-- FaceId 2
-- >>> leftFace (dart 2 "+1") myGraph
-- FaceId 2
-- >>> leftFace (dart 0 "+1") myGraph
-- FaceId 0
--
-- running time: $O(1)$.
leftFace     :: Dart s -> PlanarGraph s w v e f -> FaceId s w
leftFace d g = FaceId . headOf d $ dual g


-- | The face to the right of the dart
--
-- >>> rightFace (dart 1 "+1") myGraph
-- FaceId 2
-- >>> rightFace (dart 1 "-1") myGraph
-- FaceId 1
-- >>> rightFace (dart 2 "+1") myGraph
-- FaceId 1
-- >>> rightFace (dart 0 "+1") myGraph
-- FaceId 1
--
-- running time: $O(1)$.
rightFace     :: Dart s -> PlanarGraph s w v e f -> FaceId s w
rightFace d g = FaceId . tailOf d $ dual g


-- | The darts bounding this face, for internal faces in clockwise order, for
-- the outer face in counter clockwise order.
--
--
-- running time: $O(k)$, where $k$ is the output size.
boundary              :: FaceId s w -> PlanarGraph s w v e f -> V.Vector (Dart s)
boundary (FaceId v) g = incidentEdges v $ dual g


-- | The vertices bounding this face, for internal faces in clockwise order, for
-- the outer face in counter clockwise order.
--
--
-- running time: $O(k)$, where $k$ is the output size.
boundaryVertices     :: FaceId s w -> PlanarGraph s w v e f -> V.Vector (VertexId s w)
boundaryVertices f g = fmap (flip tailOf g) $ boundary f g

--------------------------------------------------------------------------------
-- Testing stuff

-- testPerm :: Permutation (Dart s)
-- testPerm = let (a:b:c:d:e:g:_) = take 6 [Arc 0..]
--            in toCycleRep 12 [ [ Dart a Negative
--                               , Dart c Positive
--                               , Dart b Positive
--                               , Dart a Positive
--                               ]
--                             , [ Dart e Negative
--                               , Dart b Negative
--                               , Dart d Negative
--                               , Dart g Positive
--                               ]
--                             , [ Dart e Positive
--                               , Dart d Positive
--                               , Dart c Negative
--                               ]
--                             , [ Dart g Negative
--                               ]
--                             ]

data Test

-- testG :: PlanarGraph Test Primal_ () String ()
-- testG = planarGraph' [ [ (Dart aA Negative, "a-")
--                        , (Dart aC Positive, "c+")
--                        , (Dart aB Positive, "b+")
--                        , (Dart aA Positive, "a+")
--                        ]
--                      , [ (Dart aE Negative, "e-")
--                        , (Dart aB Negative, "b-")
--                        , (Dart aD Negative, "d-")
--                        , (Dart aG Positive, "g+")
--                        ]
--                      , [ (Dart aE Positive, "e+")
--                        , (Dart aD Positive, "d+")
--                        , (Dart aC Negative, "c-")
--                        ]
--                      , [ (Dart aG Negative, "g-")
--                        ]
--                      ]
--   where
--     (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]


--------------------------------------------------------------------------------



-- type ArcID = Int

-- -- | ST' is a strict triple (m,a,x) containing:
-- --
-- -- - m: a Map, mapping edges, represented by a pair of vertexId's (u,v) with
-- --            u < v, to arcId's.
-- -- - a: the next available unused arcID
-- -- - x: the data value we are interested in computing
-- type ST' a = ST (SM.Map (VertexID,VertexID) ArcID) ArcID a
