module HGeometry.LowerEnvelope.AdjListForm
  ( LowerEnvelope(LowerEnvelope)
  , theUnboundedVertex, boundedVertices
  ) where


--------------------------------------------------------------------------------

import           Control.Lens
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           HGeometry.Foldable.Sort
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line
import           HGeometry.Line.LineEQ
import           HGeometry.LowerEnvelope.Type
import           HGeometry.Point
import           HGeometry.Properties
import           Hiraffe.Graph

--------------------------------------------------------------------------------
-- * Data type defining a lower envelope

-- | The lower envelope in adjacencylist form.
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


-- | The unbounded vertex, which by definition will have index 0
newtype UnboundedVertex plane = UnboundedVertex { _incidentEdgesU :: Seq.Seq (Edge' plane) }
                              deriving (Show,Eq)

-- | Vertices in of the lower envelope in adjacencylist form.
type BoundedVertex = BoundedVertexF Seq.Seq


class HasIncidentEdges t plane | t -> plane where
  incidentEdges' :: Lens' t (Seq.Seq (Edge' plane))

instance HasIncidentEdges (UnboundedVertex plane) plane where
  incidentEdges' = coerced

instance HasIncidentEdges (BoundedVertex plane) plane where
  incidentEdges' = lens _incidentEdgesB (\(Vertex p d _) es -> Vertex p d es)

-- instance HasIncidentEdges (Vertex (LowerEnvelope plane)) plane where
--   incidentEdges' = undefined -- pick either incidentEdges on the left or on the right thing.

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

instance HasEdges' (LowerEnvelope plane) where
  type Edge   (LowerEnvelope plane) = Edge' plane
  type EdgeIx (LowerEnvelope plane) = ( VertexIx (LowerEnvelope plane)
                                      , VertexIx (LowerEnvelope plane)
                                      )
  edgeAt (u,v) = undefined -- vertexAt u.incidentEdges'.first v

instance HasEdges (LowerEnvelope plane) (LowerEnvelope plane) where
  edges = undefined

-- FIXME: I guess strictly speaking the lower envelope is a multigraph: in case
-- the lower envelope is a bunch of parallel edges connecting v_infty to v_infty
-- for example.

-- instance Graph_ (LowerEnvelope plane) where


--------------------------------------------------------------------------------



-- instance Semigroup (LowerEnvelope plane) where
--   (LowerEnvelope u vs) <> (LowerEnvelope u' vs') = LowerEnvelope undefined undefined
  -- main idea would be to insert the vertices of vs' into vs this
  -- requires testing if a vertex already exists, and shifting it if
  -- not.  this should run in O(log n) time per edge.
  --
  -- and therefore in O(l log l + r log l) = O(n log n) time.  hmm, I
  -- guess for iterative merging we cannot afford the "l" term.  I
  -- guess we need it only to maintain some Map from location -> id.
  -- so maybe we can maintain that separately.
