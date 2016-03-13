{-# LANGUAGE TemplateHaskell #-}
module Data.PlanarGraph where

import Data.Permutation
import Data.Maybe
import Control.Monad(join, forM)
import Control.Lens
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.CircularList as C

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

newtype Arc = Arc { _unArc :: Int } deriving (Eq,Ord,Enum,Bounded)
makeLenses ''Arc

instance Show Arc where
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
data Dart = Dart { _arc       :: !Arc
                 , _direction :: !Direction
                 } deriving (Eq,Ord)
makeLenses ''Dart



instance Show Dart where
  show (Dart a d) = "Dart (" ++ show a ++ ") " ++ show d

-- | Get the twin of this dart (edge)
--
-- >>> twin (dart 0 "+1")
-- Dart (Arc 0) -1
-- >>> twin (dart 0 "-1")
-- Dart (Arc 0) +1
twin            :: Dart -> Dart
twin (Dart a d) = Dart a (rev d)

-- | test if a dart is Positive
isPositive   :: Dart -> Bool
isPositive d = d^.direction == Positive


instance Enum Dart where
  toEnum x
    | even x    = Dart (Arc $ x `div` 2)       Positive
    | otherwise = Dart (Arc $ (x `div` 2) + 1) Negative
  -- get the back edge by adding one

  fromEnum (Dart (Arc i) d) = case d of
                                Positive -> 2*i
                                Negative -> 2*i + 1


-- | The space in which the graph lives
data Space = Primal_ | Dual_ deriving (Show,Eq)

type family Dual (sp :: Space) where
  Dual Primal_ = Dual_
  Dual Dual_   = Primal_


newtype VertexId (sp :: Space) = VertexId { _unVertexId :: Int } deriving (Eq,Ord)

instance Show (VertexId sp) where
  show (VertexId i) = "VertexId " ++ show i





-- | A Planar graph with bidirected edges. I.e. the edges (darts) are directed,
-- however, for every directed edge, the edge in the oposite direction is also
-- in the graph.
--
-- The orbits in the embedding are assumed to be in counterclockwise order.
newtype PlanarGraph (sp :: Space) = PlanarGraph { _embedding :: Permutation Dart }
                                  deriving (Show,Eq)
makeLenses ''PlanarGraph

-- | Construct a planar graph
planarGraph :: Permutation Dart -> PlanarGraph Primal_
planarGraph = PlanarGraph


-- | Enumerate all vertices
vertices   :: PlanarGraph sp -> V.Vector (VertexId sp)
vertices g = fmap VertexId $ V.enumFromN 0 (V.length (g^.embedding.orbits)-1)

-- | Enumerate all darts
darts :: PlanarGraph sp -> V.Vector Dart
darts = elems . _embedding

-- | Enumerate all edges. We report only the Positive darts
edges :: PlanarGraph sp -> V.Vector Dart
edges = V.filter isPositive . darts




-- | The tail of a dart, i.e. the vertex this dart is leaving from
--
tailOf     :: Dart -> PlanarGraph sp -> VertexId sp
tailOf d g = VertexId . fst $ lookupIdx (g^.embedding) d

-- | The vertex this dart is heading in to
headOf   :: Dart -> PlanarGraph sp -> VertexId sp
headOf d = tailOf (twin d)

-- | All edges incident to vertex v, in counterclockwise order around v.
incidentEdges                :: VertexId sp -> PlanarGraph sp -> V.Vector Dart
incidentEdges (VertexId v) g = g^.embedding.orbits.ix' v

-- | All incoming edges incident to vertex v, in counterclockwise order around v.
incomingEdges     :: VertexId sp -> PlanarGraph sp -> V.Vector Dart
incomingEdges v g = V.filter (not . isPositive) $ incidentEdges v g

-- | All outgoing edges incident to vertex v, in counterclockwise order around v.
outgoingEdges     :: VertexId sp -> PlanarGraph sp -> V.Vector Dart
outgoingEdges v g = V.filter isPositive $ incidentEdges v g






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
dual   :: PlanarGraph sp -> PlanarGraph (Dual sp)
dual g = let perm = g^.embedding
         in PlanarGraph $ cycleRep (elems perm) (apply perm . twin)

--
newtype FaceId sp = FaceId { _unFaceId :: VertexId (Dual sp) } deriving (Eq,Ord)

instance Show (FaceId sp) where
  show (FaceId (VertexId i)) = "FaceId " ++ show i

-- | Enumerate all faces in the planar graph
faces :: PlanarGraph sp -> V.Vector (FaceId sp)
faces = fmap FaceId . vertices . dual


-- >>> leftFace (dart 1 "+1") myGraph
-- FaceId 1
-- >>> leftFace (dart 1 "-1") myGraph
-- FaceId 2
-- >>> leftFace (dart 2 "+1") myGraph
-- FaceId 2
-- >>> leftFace (dart 0 "+1") myGraph
-- FaceId 0
leftFace     :: Dart -> PlanarGraph sp -> FaceId sp
leftFace d g = FaceId . headOf d $ dual g


-- >>> rightFace (dart 1 "+1") myGraph
-- FaceId 2
-- >>> rightFace (dart 1 "-1") myGraph
-- FaceId 1
-- >>> rightFace (dart 2 "+1") myGraph
-- FaceId 1
-- >>> rightFace (dart 0 "+1") myGraph
-- FaceId 1
rightFace     :: Dart -> PlanarGraph sp -> FaceId sp
rightFace d g = FaceId . tailOf d $ dual g


-- | The darts bounding this face, for internal faces in clockwise order, for
-- the outer face in counter clockwise order.
--
--
boundary     :: FaceId sp -> PlanarGraph sp -> V.Vector Dart
boundary (FaceId v) g = incidentEdges v $ dual g



testG = planarGraph testPerm
testG' = dual testG



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
testx = (myAns, myAns == answer)
  where
   myAns = (dual $ planarGraph myEmbedding)^.embedding.orbits

   (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]
   myEmbedding = toCycleRep 12 [ [ Dart aA Negative
                                 , Dart aC Positive
                                 , Dart aB Positive
                                 , Dart aA Positive
                                 ]
                               , [ Dart aE Negative
                                 , Dart aB Negative
                                 , Dart aD Negative
                                 , Dart aG Positive
                                 ]
                               , [ Dart aE Positive
                                 , Dart aD Positive
                                 , Dart aC Negative
                                 ]
                               , [ Dart aG Negative
                                 ]
                               ]
   dart i s = Dart (Arc i) (read s)
   fromList = V.fromList
   answer = fromList [ fromList [dart 0 "-1"]
                     , fromList [dart 2 "+1",dart 4 "+1",dart 1 "-1",dart 0 "+1"]
                     , fromList [dart 1 "+1",dart 3 "-1",dart 2 "-1"]
                     , fromList [dart 4 "-1",dart 3 "+1",dart 5 "+1",dart 5 "-1"]
                     ]
