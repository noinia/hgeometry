{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlanarGraph.Core
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing connected planar graphs
--------------------------------------------------------------------------------
module Data.PlanarGraph.Core where


import           Control.DeepSeq
import           Control.Lens hiding ((.=))
import           Control.Monad.State.Strict
import           Data.Aeson
import qualified Data.Foldable as F
import           Data.Permutation
import           Data.PlanarGraph.Dart
import           Data.Type.Equality (gcastWith, (:~:)(..))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           GHC.Generics (Generic)
import           Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- $setup
-- >>> :{
-- let dart i s = Dart (Arc i) (read s)
--     (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]
--     myGraph :: PlanarGraph () Primal () String ()
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
--
--
-- This represents the following graph. Note that the graph is undirected, the
-- arrows are just to indicate what the Positive direction of the darts is.
--
-- ![myGraph](docs/Data/PlanarGraph/testG.png)

--------------------------------------------------------------------------------
-- * Representing The World

-- | The world in which the graph lives
data World = Primal | Dual deriving (Show,Eq)

-- | We can take the dual of a world. For the Primal this gives us the Dual,
-- for the Dual this gives us the Primal.
type family DualOf (sp :: World) where
  DualOf Primal = Dual
  DualOf Dual   = Primal

-- | The Dual of the Dual is the Primal.
dualDualIdentity :: forall w. DualOf (DualOf w) :~: w
dualDualIdentity = unsafeCoerce Refl
          -- manual proof:
          --    DualOf (DualOf Primal) = Primal
          --    DualOf (DualOf Dual)   = Dual


--------------------------------------------------------------------------------
-- * VertexId's

-- | A vertex in a planar graph. A vertex is tied to a particular planar graph
-- by the phantom type s, and to a particular world w.
newtype VertexId s (w :: World) = VertexId { _unVertexId :: Int }
                                deriving (Eq,Ord,Enum,ToJSON,FromJSON,Generic,NFData)
-- VertexId's are in the range 0...|orbits|-1

-- | Shorthand for vertices in the primal.
type VertexId' s = VertexId s Primal

unVertexId :: Getter (VertexId s w) Int
unVertexId = to _unVertexId

instance Show (VertexId s w) where
  show (VertexId i) = "VertexId " ++ show i

--------------------------------------------------------------------------------
-- * FaceId's

-- | The type to represent FaceId's
newtype FaceId s w = FaceId { _unFaceId :: VertexId s (DualOf w) }
                   deriving (Eq,Ord,Enum,ToJSON,FromJSON)

-- | Shorthand for FaceId's in the primal.
type FaceId' s = FaceId s Primal

instance Show (FaceId s w) where
  show (FaceId (VertexId i)) = "FaceId " ++ show i


--------------------------------------------------------------------------------
-- * The graph type itself

-- | A *connected* Planar graph with bidirected edges. I.e. the edges (darts) are
-- directed, however, for every directed edge, the edge in the oposite
-- direction is also in the graph.
--
-- The types v, e, and f are the are the types of the data associated with the
-- vertices, edges, and faces, respectively.
--
-- The orbits in the embedding are assumed to be in counterclockwise
-- order. Therefore, every dart directly bounds the face to its right.
data PlanarGraph s (w :: World) v e f = PlanarGraph { _embedding   :: Permutation (Dart s)
                                                    , _vertexData  :: V.Vector v
                                                    , _rawDartData :: V.Vector e
                                                    , _faceData    :: V.Vector f
                                                    , _dual        :: PlanarGraph s (DualOf w) f e v
                                                    } deriving (Generic)

instance (Show v, Show e, Show f) => Show (PlanarGraph s w v e f) where
  show (PlanarGraph e v r f _) = unwords [ "PlanarGraph"
                                         , "embedding =", show e
                                         , ", vertexData =", show v
                                         , ", rawDartData =", show r
                                         , ", faceData =", show f
                                         ]

instance (Eq v, Eq e, Eq f) => Eq (PlanarGraph s w v e f) where
  (PlanarGraph e v r f _) == (PlanarGraph e' v' r' f' _) =  e == e' && v == v'
                                                         && r == r' && f == f'



-- ** lenses and getters

-- | Get the embedding, represented as a permutation of the darts, of this
-- graph.
embedding :: Getter (PlanarGraph s w v e f) (Permutation (Dart s))
embedding = to _embedding

vertexData :: Lens (PlanarGraph s w v e f) (PlanarGraph s w v' e f)
                   (V.Vector v) (V.Vector v')
vertexData = lens _vertexData (\g vD -> updateData (const vD) id id g)

rawDartData :: Lens (PlanarGraph s w v e f) (PlanarGraph s w v e' f)
                    (V.Vector e) (V.Vector e')
rawDartData = lens _rawDartData (\g dD -> updateData id (const dD) id g)

faceData :: Lens (PlanarGraph s w v e f) (PlanarGraph s w v e f')
                 (V.Vector f) (V.Vector f')
faceData = lens _faceData (\g fD -> updateData id id (const fD) g)

-- | Get the dual graph of this graph.
dual :: Getter (PlanarGraph s w v e f) (PlanarGraph s (DualOf w) f e v)
dual = to _dual


-- FIXME: So I guess the two darts associated with an edge can store different
-- data. This is useful. Make sure that works as expected.

-- | lens to access the Dart Data
--
--
dartData :: Lens (PlanarGraph s w v e f) (PlanarGraph s w v e' f)
                 (V.Vector (Dart s, e))  (V.Vector (Dart s, e'))
dartData = lens darts (\g dD -> updateData id (const $ reorderEdgeData dD) id g)

-- | edgeData is just an alias for 'dartData'
edgeData :: Lens (PlanarGraph s w v e f) (PlanarGraph s w v e' f)
                 (V.Vector (Dart s, e)) (V.Vector (Dart s, e'))
edgeData = dartData

-- | Helper function to update the data in a planar graph. Takes care to update
-- both the data in the original graph as well as in the dual.
updateData :: forall s w v e f v' e' f'
           .  (V.Vector v -> V.Vector v')
           -> (V.Vector e -> V.Vector e')
           -> (V.Vector f -> V.Vector f')
           -> PlanarGraph s w v  e  f
           -> PlanarGraph s w v' e' f'
updateData = gcastWith proof updateData'
  where
    proof :: DualOf (DualOf w) :~: w
    proof = dualDualIdentity

-- | The function that does the actual work for 'updateData'
updateData'  :: (DualOf (DualOf w) ~ w)
             => (V.Vector v -> V.Vector v')
             -> (V.Vector e -> V.Vector e')
             -> (V.Vector f -> V.Vector f')
             -> PlanarGraph s w v  e  f
             -> PlanarGraph s w v' e' f'
updateData' fv fe ff (PlanarGraph em vtxData dData fData dg) = g'
  where
    vtxData' = fv vtxData
    dData'   = fe dData
    fData'   = ff fData

    g'       = PlanarGraph em              vtxData' dData' fData'   dg'
    dg'      = PlanarGraph (dg^.embedding) fData'   dData' vtxData' g'


-- | Reorders the edge data to be in the right order to set edgeData
reorderEdgeData    :: Foldable f => f (Dart s, e) -> V.Vector e
reorderEdgeData ds = V.create $ do
                                  v <- MV.new (F.length ds)
                                  forM_ (F.toList ds) $ \(d,x) ->
                                    MV.write v (fromEnum d) x
                                  pure v

-- | Traverse the vertices
--
-- >>> (^.vertexData) <$> traverseVertices (\i x -> Just (i,x)) myGraph
-- Just [(VertexId 0,()),(VertexId 1,()),(VertexId 2,()),(VertexId 3,())]
-- >>> traverseVertices (\i x -> print (i,x)) myGraph >> pure ()
-- (VertexId 0,())
-- (VertexId 1,())
-- (VertexId 2,())
-- (VertexId 3,())
traverseVertices   :: Applicative m
                   => (VertexId s w -> v -> m v')
                   -> PlanarGraph s w v e f
                   -> m (PlanarGraph s w v' e f)
traverseVertices f = itraverseOf (vertexData.itraversed) (\i -> f (VertexId i))

-- | Traverses the darts
--
-- >>> traverseDarts (\d x -> print (d,x)) myGraph >> pure ()
-- (Dart (Arc 0) +1,"a+")
-- (Dart (Arc 0) -1,"a-")
-- (Dart (Arc 1) +1,"b+")
-- (Dart (Arc 1) -1,"b-")
-- (Dart (Arc 2) +1,"c+")
-- (Dart (Arc 2) -1,"c-")
-- (Dart (Arc 3) +1,"d+")
-- (Dart (Arc 3) -1,"d-")
-- (Dart (Arc 4) +1,"e+")
-- (Dart (Arc 4) -1,"e-")
-- (Dart (Arc 5) +1,"g+")
-- (Dart (Arc 5) -1,"g-")
traverseDarts   :: Applicative m
                => (Dart s -> e -> m e')
                -> PlanarGraph s w v e f
                -> m (PlanarGraph s w v e' f)
traverseDarts f = itraverseOf (rawDartData.itraversed) (\i -> f (toEnum i))

-- | Traverses the faces
--
-- >>> traverseFaces (\i x -> print (i,x)) myGraph >> pure ()
-- (FaceId 0,())
-- (FaceId 1,())
-- (FaceId 2,())
-- (FaceId 3,())
traverseFaces   :: Applicative m
                => (FaceId s w -> f -> m f')
                -> PlanarGraph s w v e f
                -> m (PlanarGraph s w v e f')
traverseFaces f = itraverseOf (faceData.itraversed) (\i -> f (FaceId $ VertexId i))

--------------------------------------------------------------------------------
-- ** Constructing a Planar graph

-- | Construct a planar graph
--
-- running time: \(O(n)\).
planarGraph'      :: Permutation (Dart s) -> PlanarGraph s w () () ()
planarGraph' perm = pg
  where
    pg = PlanarGraph perm vData eData fData (computeDual pg)
        -- note the lazy calculation of computeDual that refers to pg itself
    d  = size perm
    e  = d `div` 2
    v  = V.length (perm^.orbits)
    f  = e - v + 2

    vData  = V.replicate v ()
    eData  = V.replicate d ()
    fData  = V.replicate f ()

-- | Construct a planar graph, given the darts in cyclic order around each
-- vertex.
--
-- running time: \(O(n)\).
planarGraph    :: [[(Dart s,e)]] -> PlanarGraph s Primal () e ()
planarGraph ds = (planarGraph' perm)&dartData .~ (V.fromList . concat $ ds)
  where
    n     = sum . map length $ ds
    perm  = toCycleRep n $ map (map fst) ds




-- | Produces the adjacencylists for all vertices in the graph. For every vertex, the
-- adjacent vertices are given in counter clockwise order.
--
-- Note that in case a vertex u as a self loop, we have that this vertexId occurs
-- twice in the list of neighbours, i.e.: u : [...,u,..,u,...]. Similarly, if there are
-- multiple darts between a pair of edges they occur multiple times.
--
-- running time: \(O(n)\)
toAdjacencyLists    :: PlanarGraph s w v e f -> [(VertexId s w, V.Vector (VertexId s w))]
toAdjacencyLists pg = map (\u -> (u,neighboursOf u pg)) . V.toList . vertices' $ pg
-- TODO: something weird happens when we have self-loops here.


--------------------------------------------------------------------------------
-- ** Convenience functions

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
darts g = (\d -> (d,g^.dataOf d)) <$> darts' g

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
-- running time: \(O(1)\)
tailOf     :: Dart s -> PlanarGraph s w v e f -> VertexId s w
tailOf d g = VertexId . fst $ lookupIdx (g^.embedding) d

-- | The vertex this dart is heading in to
--
-- running time: \(O(1)\)
headOf   :: Dart s -> PlanarGraph s w v e f -> VertexId s w
headOf d = tailOf (twin d)

-- | endPoints d g = (tailOf d g, headOf d g)
--
-- running time: \(O(1)\)
endPoints :: Dart s -> PlanarGraph s w v e f -> (VertexId s w, VertexId s w)
endPoints d g = (tailOf d g, headOf d g)


-- | All edges incident to vertex v, in counterclockwise order around v.
--
--
--
-- running time: \(O(k)\), where \(k\) is the output size
incidentEdges                :: VertexId s w -> PlanarGraph s w v e f
                             -> V.Vector (Dart s)
incidentEdges (VertexId v) g = g^?!embedding.orbits.ix v
  -- TODO: The Delaunay triang. stuff seems to produce these in clockwise order instead

-- | All edges incident to vertex v in incoming direction
-- (i.e. pointing into v) in counterclockwise order around v.
--
-- running time: \(O(k)\), where \(k) is the total number of incident edges of v
incomingEdges     :: VertexId s w -> PlanarGraph s w v e f -> V.Vector (Dart s)
incomingEdges v g = orient <$> incidentEdges v g
  where
    orient d = if headOf d g == v then d else twin d

-- | All edges incident to vertex v in outgoing direction
-- (i.e. pointing away from v) in counterclockwise order around v.
--
-- running time: \(O(k)\), where \(k) is the total number of incident edges of v
outgoingEdges     :: VertexId s w -> PlanarGraph s w v e f -> V.Vector (Dart s)
outgoingEdges v g = orient <$> incidentEdges v g
  where
    orient d = if tailOf d g == v then d else twin d


-- | Gets the neighbours of a particular vertex, in counterclockwise order
-- around the vertex.
--
-- running time: \(O(k)\), where \(k\) is the output size
neighboursOf     :: VertexId s w -> PlanarGraph s w v e f -> V.Vector (VertexId s w)
neighboursOf v g = flip tailOf g <$> incomingEdges v g

-- | Given a dart d that points into some vertex v, report the next dart in the
-- cyclic order around v.
--
-- running time: \(O(1)\)
nextIncidentEdge     :: Dart s -> PlanarGraph s w v e f -> Dart s
nextIncidentEdge d g = let perm  = g^.embedding
                           (i,j) = lookupIdx perm d
                       in next (perm^?!orbits.ix i) j


-- | Given a dart d that points into some vertex v, report the next dart in the
-- cyclic order around v.
--
-- running time: \(O(1)\)
prevIncidentEdge     :: Dart s -> PlanarGraph s w v e f -> Dart s
prevIncidentEdge d g = let perm  = g^.embedding
                           (i,j) = lookupIdx perm d
                       in previous (perm^?!orbits.ix i) j


--------------------------------------------------------------------------------
-- * Access data


class HasDataOf g i where
  type DataOf g i
  -- | get the data associated with the value i.
  --
  -- running time: \(O(1)\) to read the data, \(O(n)\) to write it.
  dataOf :: i -> Lens' g (DataOf g i)

instance HasDataOf (PlanarGraph s w v e f) (VertexId s w) where
  type DataOf (PlanarGraph s w v e f) (VertexId s w) = v
  dataOf (VertexId i) = vertexData.singular (ix i)

instance HasDataOf (PlanarGraph s w v e f) (Dart s) where
  type DataOf (PlanarGraph s w v e f) (Dart s) = e
  dataOf d = rawDartData.singular (ix $ fromEnum d)

instance HasDataOf (PlanarGraph s w v e f) (FaceId s w) where
  type DataOf (PlanarGraph s w v e f) (FaceId s w) = f
  dataOf (FaceId (VertexId i)) = faceData.singular (ix i)


-- | Data corresponding to the endpoints of the dart
endPointDataOf   :: Dart s -> Getter (PlanarGraph s w v e f) (v,v)
endPointDataOf d = to $ endPointData d


-- | Data corresponding to the endpoints of the dart
--
-- running time: \(O(1)\)
endPointData     :: Dart s -> PlanarGraph s w v e f -> (v,v)
endPointData d g = let (u,v) = endPoints d g in (g^.dataOf u, g^.dataOf v)


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
--  in (computeDual myGraph)^.embedding.orbits == answer
-- :}
-- True
--
-- running time: \(O(n)\).
computeDual :: forall s w v e f. PlanarGraph s w v e f -> PlanarGraph s (DualOf w) f e v
computeDual = gcastWith proof computeDual'
  where
    proof :: DualOf (DualOf w) :~: w
    proof = dualDualIdentity

-- | Does the actual work for dualGraph
computeDual'   :: (DualOf (DualOf w) ~ w)
               => PlanarGraph s w v e f -> PlanarGraph s (DualOf w) f e v
computeDual' g = dualG
  where
    perm  = g^.embedding
    dualG = PlanarGraph (cycleRep (elems perm) (apply perm . twin))
                        (g^.faceData)
                        (g^.rawDartData)
                        (g^.vertexData)
                        g
