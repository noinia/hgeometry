{-# LANGUAGE UndecidableInstances #-}
module HGeometry.LowerEnvelope.AdjListForm
  ( LowerEnvelope(LowerEnvelope)
  , theUnboundedVertex, boundedVertices

  , BoundedVertexF(Vertex)
  , location, definers, location2

  , LEEdge(Edge)

  ) where


--------------------------------------------------------------------------------

import           Control.Lens
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           HGeometry.Ext
import           HGeometry.Foldable.Sort
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line
import           HGeometry.Line.LineEQ
import           HGeometry.LowerEnvelope.Type
import qualified HGeometry.LowerEnvelope.VertexForm as VertexForm
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




singleton   :: VertexForm.LEVertex plane -> LowerEnvelope plane
singleton v = undefined


fromVertexForm :: VertexForm.VertexForm plane -> LowerEnvelope plane
fromVertexForm = undefined


--------------------------------------------------------------------------------


-- | The unbounded vertex, which by definition will have index 0
newtype UnboundedVertex plane = UnboundedVertex { _incidentEdgesU :: Seq.Seq (LEEdge plane) }
                              deriving (Show,Eq)

--------------------------------------------------------------------------------

-- | Vertices in of the lower envelope in adjacencylist form.
type BoundedVertex = BoundedVertexF Seq.Seq



fromLEVertex                              :: VertexForm.LEVertex plane -> BoundedVertex plane
fromLEVertex (VertexForm.LEVertex v defs) = Vertex v defs es
  where
    es = undefined

orderPlanesAround      :: Point 3 r -> Set.Set plane -> Seq.Seq plane
orderPlanesAround v hs = undefined


newtype HalfLine r = HalfLine (LinePV 2 r)
                   deriving (Show,Eq)

outgoingUnboundedEdge               :: Point 3 r -- ^ the location of the vertex v
                                    -> (plane, plane) -- ^ the pair of planes for which to compute
                                    -- the halfine
                                    -> plane -- ^ a third half plane intersecting at v
                                    -> Maybe (HalfLine r :+ EdgeDefiners plane)
outgoingUnboundedEdge v (h1, h2) h3 = Just $ hl :+ defs
  -- todo, if there are more planes, I guess we should check if the hl is not dominated by the other
  -- planes either.
  where
    (hl :+ defs) = toHalfLineFrom (projectPoint v) h3 $ intersectionLine h1 h2

-- | convert into a halfline
toHalfLineFrom     :: Point 2 r -> plane -> LinePV 2 r :+ EdgeDefiners plane
                   -> HalfLine r :+ EdgeDefiners plane
toHalfLineFrom v l = undefined

data EdgeDefiners plane = EdgeDefiners { _leftPlane  :: plane -- above plane
                                       , _rightPlane :: plane -- below plane
                                       }

-- | Computes the line in which the two planes intersect
intersectionLine      :: plane -> plane -> LinePV 2 r :+ EdgeDefiners plane
intersectionLine h h' = undefined

--------------------------------------------------------------------------------

class HasIncidentEdges t plane | t -> plane where
  incidentEdges' :: Lens' t (Seq.Seq (LEEdge plane))

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
  type Edge   (LowerEnvelope plane) = LEEdge plane
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
