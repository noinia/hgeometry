{-# LANGUAGE UndecidableInstances #-}
module HGeometry.LowerEnvelope.Naive
  ( lowerEnvelope
  , triangulatedLowerEnvelope
  ) where

--------------------------------------------------------------------------------

import           Control.Lens
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           HGeometry.Foldable.Sort
-- import           HGeometry.HalfPlane
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line
import           HGeometry.Line.LineEQ
import           HGeometry.Point
import           HGeometry.Properties
import           Hiraffe.Graph

--------------------------------------------------------------------------------
-- * Data type defining a lower envelope

data LowerEnvelope plane =
  LowerEnvelope !(UnboundedVertex plane) (Seq.Seq (BoundedVertex plane))

deriving instance (Show plane, Show (NumType plane)) => Show (LowerEnvelope plane)
deriving instance (Eq plane, Eq (NumType plane))     => Eq   (LowerEnvelope plane)

theUnboundedVertex :: Lens' (LowerEnvelope plane) (UnboundedVertex plane)
theUnboundedVertex = lens (\(LowerEnvelope v _) -> v)
                          (\(LowerEnvelope _ vs) v -> LowerEnvelope v vs)

boundedVertices :: Lens' (LowerEnvelope plane) (Seq.Seq (BoundedVertex plane))
boundedVertices = lens (\(LowerEnvelope _ vs)    -> vs)
                       (\(LowerEnvelope u _ ) vs -> LowerEnvelope u vs)


----------------------------------------

-- | The unbounded vertex, which by definition will have index 0
newtype UnboundedVertex plane = UnboundedVertex { _incidentEdgesU :: Seq.Seq (Edge' plane) }
                              deriving (Show,Eq)

data BoundedVertex plane = Vertex { _location       :: !(Point 3 (NumType plane))
                                  , _definers       :: Set.Set plane
                                  , _incidentEdgesB :: Seq.Seq (Edge' plane)
                                  -- ^ incident edges, in CCW order.
                                  }

deriving instance (Show plane, Show (NumType plane)) => Show (BoundedVertex plane)
deriving instance (Eq plane, Eq (NumType plane))     => Eq   (BoundedVertex plane)

-- | The location of the vertex
location :: (NumType plane ~ r) => Lens' (BoundedVertex plane) (Point 3 r)
location = lens _location (\v l -> v { _location = l })

-- | Projected 2d location of the point
location2 :: (NumType plane ~ r) => Getter (BoundedVertex plane) (Point 2 r)
location2 = location . to projectPoint

-- | The three planes defining the vertex
definers :: Lens' (BoundedVertex plane) (Set.Set plane)
definers = lens _definers (\v ds -> v { _definers = ds })


class HasIncidentEdges t plane | t -> plane where
  incidentEdges' :: Lens' t (Seq.Seq (Edge' plane))

instance HasIncidentEdges (UnboundedVertex plane) plane where
  incidentEdges' = coerced

instance HasIncidentEdges (BoundedVertex plane) plane where
  incidentEdges' = lens _incidentEdgesB (\(Vertex p d _) es -> Vertex p d es)

-- instance HasIncidentEdges (Vertex (LowerEnvelope plane)) plane where
--   incidentEdges' = undefined -- pick either incidentEdges on the left or on the right thing.

type VertexID = Int
instance HasVertices' (LowerEnvelope plane) where
  type Vertex   (LowerEnvelope plane) = Either (UnboundedVertex plane) (BoundedVertex plane)
  type VertexIx (LowerEnvelope plane) = VertexID

  -- | note, trying to assign the unbounded vertex to something with index >0 is an error
  vertexAt = \case
    0 -> undefined
    i -> undefined -- boundedVertices.ix (i+1)

instance HasVertices (LowerEnvelope plane) (LowerEnvelope plane') where
  vertices = undefined

----------------------------------------

data Edge' plane = Edge { _destination :: {-# UNPACK #-}!VertexID
                        , _leftPlane   :: plane
                        , _rightPlane  :: plane
                        } deriving (Show,Eq,Ord)

instance HasEdges' (LowerEnvelope plane) where
  type Edge   (LowerEnvelope plane) = Edge' plane
  type EdgeIx (LowerEnvelope plane) = ( VertexIx (LowerEnvelope plane)
                                      , VertexIx (LowerEnvelope plane)
                                      )
  edgeAt (u,v) = undefined -- vertexAt u.incidentEdges'.first v

instance HasEdges (LowerEnvelope plane) (LowerEnvelope plane) where
  edges = undefined

----------------------------------------

-- FIXME: I guess strictly speaking the lower envelope is a multigraph: in case
-- the lower envelope is a bunch of parallel edges connecting v_infty to v_infty
-- for example.

-- instance Graph_ (LowerEnvelope plane) where


--------------------------------------------------------------------------------

-- | simple implementation of the lower envelope.
--
-- running time: \(O(n^2\log n)\)
lowerEnvelope    :: ( Plane_ plane r
                    , Ord r, Fractional r, Foldable f, Ord plane
                    ) => f plane -> LowerEnvelope r
lowerEnvelope hs = undefined

triangulatedLowerEnvelope    :: ( Plane_ plane r
                                , Ord r, Fractional r, Foldable f
                                ) => f plane -> LowerEnvelope r
triangulatedLowerEnvelope hs = undefined


--------------------------------------------------------------------------------

{-

-- | Given two halfplanes h and h' computes the halfplane where h lies
-- vertically below h'.
liesBelowIn                                :: (Plane_ plane r, Ord r, Fractional r)
                                           => plane -> plane -> Maybe (HalfPlane r)
liesBelowIn (Plane_ a b c) (Plane_ a' b' c') = case b `compare` b' of
                                                 LT -> Just $ Above (LineEQ d e)
                                                 GT -> Just $ Below (LineEQ d e)
                                                 EQ -> case a `compare` a' of
                                                         LT -> Just $ RightOf f
                                                         GT -> Just $ LeftOf f
                                                         EQ -> Nothing
  where
    d = (a-a') / (b - b')
    e = (c-c') / (b - b')
    f = (c-c') / (a - a')

-}
