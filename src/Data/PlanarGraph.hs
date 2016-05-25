{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.PlanarGraph where

import           Control.Lens
import           Control.Monad (join, forM, forM_, when, filterM)
import           Control.Monad.ST (ST,runST)
import           Data.Ext
import           Data.Maybe
import           Data.Permutation
import           Data.Tree
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- $setup
-- >>> :{
--  let
--    (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]
--    myEmbedding = toCycleRep 12 [ [ Dart aA Negative
--                                  , Dart aC Positive
--                                  , Dart aB Positive
--                                  , Dart aA Positive
--                                  ]
--                                , [ Dart aE Negative
--                                  , Dart aB Negative
--                                  , Dart aD Negative
--                                  , Dart aG Positive
--                                  ]
--                                , [ Dart aE Positive
--                                  , Dart aD Positive
--                                  , Dart aC Negative
--                                  ]
--                                , [ Dart aG Negative
--                                  ]
--                                ]
--    myGraph = planarGraph myEmbedding
--    dart i s = Dart (Arc i) (read s)
-- :}

-- TODO: Add a fig. of the Graph


--------------------------------------------------------------------------------

newtype Arc s = Arc { _unArc :: Int } deriving (Eq,Ord,Enum,Bounded)
makeLenses ''Arc

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


-- | The space in which the graph lives
data World = Primal_ | Dual_ deriving (Show,Eq)

type family Dual (sp :: World) where
  Dual Primal_ = Dual_
  Dual Dual_   = Primal_


newtype VertexId s (w :: World) = VertexId { _unVertexId :: Int } deriving (Eq,Ord,Enum)
-- VertexId's are in the range 0...|orbits|-1

instance Show (VertexId s w) where
  show (VertexId i) = "VertexId " ++ show i





-- | A *connected* Planar graph with bidirected edges. I.e. the edges (darts) are
-- directed, however, for every directed edge, the edge in the oposite
-- direction is also in the graph.
--
-- The orbits in the embedding are assumed to be in counterclockwise order.
data PlanarGraph s (w :: World) v e f = PlanarGraph { _embedding  :: Permutation (Dart s)
                                                    , _vertexData :: V.Vector v
                                                    , _edgeData   :: V.Vector e
                                                    , _faceData   :: V.Vector f
                                                    }
                                      deriving (Show,Eq)
makeLenses ''PlanarGraph

-- | Construct a planar graph
planarGraph      :: Permutation (Dart s) -> PlanarGraph s Primal_ () () ()
planarGraph perm = PlanarGraph perm vData eData fData
  where
    d = size perm
    e = d `div` 2
    v = V.length (perm^.orbits)
    f = e - v + 2

    vData  = V.replicate v ()
    eData  = V.replicate d ()
    fData  = V.replicate f ()

-- | Get the number of vertices
numVertices :: PlanarGraph s w v e f -> Int
numVertices g = V.length (g^.embedding.orbits)

-- | Get the number of Edges
numEdges :: PlanarGraph s w v e f -> Int
numEdges g = size (g^.embedding) `div` 2

-- | Get the number of faces
numFaces   :: PlanarGraph s w v e f -> Int
numFaces g = numEdges g - numVertices g + 2


-- | Enumerate all vertices
vertices   :: PlanarGraph s w v e f -> V.Vector (VertexId s w)
vertices g = fmap VertexId $ V.enumFromN 0 (V.length (g^.embedding.orbits))

-- | Enumerate all darts
darts :: PlanarGraph s w v e f -> V.Vector (Dart s)
darts = elems . _embedding

-- | Enumerate all edges. We report only the Positive darts
edges :: PlanarGraph s w v e f -> V.Vector (Dart s)
edges = V.filter isPositive . darts




-- | The tail of a dart, i.e. the vertex this dart is leaving from
--
tailOf     :: Dart s -> PlanarGraph s w v e f -> VertexId s w
tailOf d g = VertexId . fst $ lookupIdx (g^.embedding) d

-- | The vertex this dart is heading in to
headOf   :: Dart s -> PlanarGraph s w v e f -> VertexId s w
headOf d = tailOf (twin d)

-- | endPoints d g = (tailOf d g, headOf d g)
endPoints :: Dart s -> PlanarGraph s w v e f -> (VertexId s w, VertexId s w)
endPoints d g = (tailOf d g, headOf d g)


-- | All edges incident to vertex v, in counterclockwise order around v.
incidentEdges                :: VertexId s w -> PlanarGraph s w v e f
                             -> V.Vector (Dart s)
incidentEdges (VertexId v) g = g^.embedding.orbits.ix' v

-- | All incoming edges incident to vertex v, in counterclockwise order around v.
incomingEdges     :: VertexId s w -> PlanarGraph s w v e f -> V.Vector (Dart s)
incomingEdges v g = V.filter (not . isPositive) $ incidentEdges v g

-- | All outgoing edges incident to vertex v, in counterclockwise order around v.
outgoingEdges     :: VertexId s w -> PlanarGraph s w v e f -> V.Vector (Dart s)
outgoingEdges v g = V.filter isPositive $ incidentEdges v g



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
vDataOf              :: VertexId s w -> Lens' (PlanarGraph s w v e f) v
vDataOf (VertexId i) = vertexData.ix' i

-- | Edge data of a given dart
eDataOf   :: Dart s -> Lens' (PlanarGraph s w v e f) e
eDataOf d = edgeData.ix' (fromEnum d)

-- | Data of a face of a given face
fDataOf                       :: FaceId s w -> Lens' (PlanarGraph s w v e f) f
fDataOf (FaceId (VertexId i)) = faceData.ix' i


-- | Data corresponding to the endpoints of the dart
endPointDataOf   :: Dart s -> Getter (PlanarGraph s w v e f) (v,v)
endPointDataOf d = to $ endPointData d


-- | Data corresponding to the endpoints of the dart
endPointData     :: Dart s -> PlanarGraph s w v e f -> (v,v)
endPointData d g = let (u,v) = endPoints d g in (g^.vDataOf u, g^.vDataOf v)

-- | Get the edge data associated with each dart
withEdgeData :: PlanarGraph s w v e f -> V.Vector (Dart s, e)
withEdgeData g = V.zip (elems $ g^.embedding) (g^.edgeData)


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
dual   :: PlanarGraph s w v e f -> PlanarGraph s (Dual w) f e v
dual g = let perm = g^.embedding
         in PlanarGraph (cycleRep (elems perm) (apply perm . twin))
                        (g^.faceData)
                        (g^.edgeData)
                        (g^.vertexData)

--
newtype FaceId s w = FaceId { _unFaceId :: VertexId s (Dual w) } deriving (Eq,Ord)

instance Show (FaceId s w) where
  show (FaceId (VertexId i)) = "FaceId " ++ show i

-- | Enumerate all faces in the planar graph
faces :: PlanarGraph s w v e f -> V.Vector (FaceId s w)
faces = fmap FaceId . vertices . dual

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
rightFace     :: Dart s -> PlanarGraph s w v e f -> FaceId s w
rightFace d g = FaceId . tailOf d $ dual g


-- | The darts bounding this face, for internal faces in clockwise order, for
-- the outer face in counter clockwise order.
--
--
boundary     :: FaceId s w -> PlanarGraph s w v e f -> V.Vector (Dart s)
boundary (FaceId v) g = incidentEdges v $ dual g



-- testG = planarGraph testPerm
-- testG' = dual testG






testPerm :: Permutation (Dart s)
testPerm = let (a:b:c:d:e:g:_) = take 6 [Arc 0..]
           in toCycleRep 12 [ [ Dart a Negative
                              , Dart c Positive
                              , Dart b Positive
                              , Dart a Positive
                              ]
                            , [ Dart e Negative
                              , Dart b Negative
                              , Dart d Negative
                              , Dart g Positive
                              ]
                            , [ Dart e Positive
                              , Dart d Positive
                              , Dart c Negative
                              ]
                            , [ Dart g Negative
                              ]
                            ]
-- testx = (myAns, myAns == answer)
--   where
--    myAns = (dual $ planarGraph myEmbedding)^.embedding.orbits

--    (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]
--    myEmbedding = toCycleRep 12 [ [ Dart aA Negative
--                                  , Dart aC Positive
--                                  , Dart aB Positive
--                                  , Dart aA Positive
--                                  ]
--                                , [ Dart aE Negative
--                                  , Dart aB Negative
--                                  , Dart aD Negative
--                                  , Dart aG Positive
--                                  ]
--                                , [ Dart aE Positive
--                                  , Dart aD Positive
--                                  , Dart aC Negative
--                                  ]
--                                , [ Dart aG Negative
--                                  ]
--                                ]
--    dart i s = Dart (Arc i) (read s)
--    fromList = V.fromList
--    answer = fromList [ fromList [dart 0 "-1"]
--                      , fromList [dart 2 "+1",dart 4 "+1",dart 1 "-1",dart 0 "+1"]
--                      , fromList [dart 1 "+1",dart 3 "-1",dart 2 "-1"]
--                      , fromList [dart 4 "-1",dart 3 "+1",dart 5 "+1",dart 5 "-1"]
--                      ]


--------------------------------------------------------------------------------
-- * Algorithms

-- | DFS on a planar graph.
--
-- Running time: O(n)
--
-- Note that since our planar graphs are always connected there is no need need
-- for dfs to take a list of start vertices.
dfs  :: forall s w v e f.
      PlanarGraph s w v e f -> VertexId s w -> Tree (VertexId s w)
dfs g = dfs' (adjacencyLists g)


-- | Transform into adjacencylist representation
adjacencyLists   :: PlanarGraph s w v e f -> AdjacencyLists s w
adjacencyLists g = V.toList . flip neighboursOf g <$> vertices g


type AdjacencyLists s w = V.Vector [VertexId s w]

dfs'          :: forall s w. AdjacencyLists s w -> VertexId s w -> Tree (VertexId s w)
dfs' g start = runST $ do
                 bv     <- UMV.replicate n False -- bit vector of marks
                 -- start will be unvisited, thus the fromJust is safe
                 fromJust <$> dfs'' bv start
  where
    n = GV.length g

    neighs              :: VertexId s w -> [VertexId s w]
    neighs (VertexId u) = g GV.! u

    visit   bv (VertexId i) = UMV.write bv i True
    visited bv (VertexId i) = UMV.read  bv i
    dfs''      :: UMV.MVector s' Bool -> VertexId s w
               -> ST s' (Maybe (Tree (VertexId s w)))
    dfs'' bv u = visited bv u >>= \case
                   True  -> pure Nothing
                   False -> do
                              visit bv u
                              Just . Node u . catMaybes <$> mapM (dfs'' bv) (neighs u)



-- " "Minimum spanning tree of the edges. The result is a rooted tree, in which
-- the nodes are the vertices in the planar graph together with the edge weight
-- of the edge to their parent. The root's weight is zero.
--
-- running time: $O(n \log n)$
mst   :: Ord e => PlanarGraph s w v e f -> Tree (VertexId s w)
mst g = makeTree g $ mstEdges g
  -- TODO: Add edges/darts to the output somehow.


-- | Computes the set of edges in the Minimum spanning tree
--
-- running time: $O(n \log n)$
mstEdges   :: Ord e => PlanarGraph s w v e f -> [Dart s]
mstEdges g = runST $ do
          uf <- new (numVertices g)
          filterM (\e -> union uf (headOf e g) (tailOf e g)) edges'
  where
    edges' = map fst . L.sortOn snd . V.toList $ V.zip (edges g) (g^.edgeData)


-- Given the underlying planar graph, and a set of edges that form a tree,
-- create the actual tree.
--
-- pre: the planar graph has at least one vertex.
makeTree   :: forall s w v e f.
              PlanarGraph s w v e f -> [Dart s] -> Tree (VertexId s w)
makeTree g = flip dfs' start . mkAdjacencyLists
  where
    n = numVertices g
    start = V.head $ vertices g

    append                  :: MV.MVector s' [a] -> VertexId s w -> a -> ST s' ()
    append v (VertexId i) x = MV.read v i >>= MV.write v i . (x:)

    mkAdjacencyLists        :: [Dart s] -> AdjacencyLists s w
    mkAdjacencyLists edges' = V.create $ do
                                vs <- MV.replicate n []
                                forM_ edges' $ \e -> do
                                  let u = headOf e g
                                      v = tailOf e g
                                  append vs u v
                                  append vs v u
                                pure vs

-- | Union find DS
newtype UF s a = UF { _unUF :: UMV.MVector s (Int,Int) }

new   :: Enum a => Int -> ST s (UF s a)
new n = do
          v <- UMV.new n
          forM_ [0..n-1] $ \i ->
            UMV.write v i (i,0)
          pure $ UF v

-- | Union the components containing x and y. Returns weather or not the two
-- components were already in the same component or not.
union               :: (Enum a, Eq a) => UF s a -> a -> a -> ST s Bool
union uf@(UF v) x y = do
                        (rx,rrx) <- find' uf x
                        (ry,rry) <- find' uf y
                        let b = rx /= ry
                            rx' = fromEnum rx
                            ry' = fromEnum ry
                        when b $ case rrx `compare` rry of
                            LT -> UMV.write v rx'  (ry',rrx)
                            GT -> UMV.write v ry' (rx',rry)
                            EQ -> do UMV.write v ry' (rx',rry)
                                     UMV.write v rx' (rx',rrx+1)
                        pure b


-- | Get the representative of the component containing x
find    :: (Enum a, Eq a) => UF s a -> a -> ST s a
find uf = fmap fst . find' uf

-- | Get the representative (and its rank) of the component containing x
find'             :: (Enum a, Eq a) => UF s a -> a -> ST s (a,Int)
find' uf@(UF v) x = do
                      (p,r) <- UMV.read v (fromEnum x) -- get my parent
                      if toEnum p == x then
                        pure (x,r) -- I am a root
                      else do
                        rt@(j,_) <- find' uf (toEnum p)  -- get the root of my parent
                        UMV.write v (fromEnum x) (fromEnum j,r)   -- path compression
                        pure rt




-- mst g = undefined

-- -- | runs MST with a given root
-- mstFrom     :: (Ord e, Monoid e)
--             => VertexId s w -> PlanarGraph s w v e f -> Tree (VertexId s w, e)
-- mstFrom r g = prims initialQ (Node (r,mempty) [])
--   where
--     update' k p q = Q.adjust (const p) k q

--     -- initial Q has the value of the root set to the zero element, and has no
--     -- parent. The others are all set to Top (and have no parent yet)
--     initialQ = update' r (ValT (mempty,Nothing))
--              . GV.foldr (\v q -> Q.insert v (Top,Nothing) q) Q.empty $ vertices g

--     prims qq t = case Q.minView qq of
--       Nothing -> t
--       Just (v Q.:-> (w,p), q) -> prims $
