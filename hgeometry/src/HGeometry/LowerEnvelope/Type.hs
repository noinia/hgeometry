{-# LANGUAGE UndecidableInstances #-}
module HGeometry.LowerEnvelope.Type
  ( VertexID
  , BoundedVertexF(Vertex)
  , location, definers, location2, incidentEdgesB
  , traverseBoundedV

  , LEEdge(Edge)
  -- , createEdge
  , destination, leftPlane, rightPlane
  , flipEdge
  ) where

import           Control.Lens
import qualified Data.Set as Set
import           HGeometry.Point
import           HGeometry.Properties

--------------------------------------------------------------------------------

-- | VertexIds are just ints
type VertexID = Int

-- | Orders the plane around their intersection point.
newtype AroundVertex plane = AroundVertex plane
                           deriving stock (Show,Eq,Functor,Foldable,Traversable)

-- instance Ord (AroundVertex plane) where
--   (AroundVertex h) `compare` (AroundVertex h') = undefined
--     where
--       -- the vertex location; aquire this using reflection somehow
--       v :: Point 2 (NumType plane)
--       v = undefined

      -- I guess we somehow compute the bisector, and figure out if h lies above the bisector or below.


-- | Data type representing the bounded vertices of the lower
-- envelope.
--
-- In the vertexform, the f will be Const (), i.e. we don't have
-- adjacency information. But in case of Adjacencylist form we can
-- implement it using a Sequence.
data BoundedVertexF f plane = Vertex { _location       :: !(Point 3 (NumType plane))
                                     , _definers       :: Set.Set plane
                                     , _incidentEdgesB :: f (LEEdge plane)
                                     -- ^ incident edges, in CCW order.
                                     }

deriving instance ( Show plane, Show (NumType plane)
                  , Show (f (LEEdge plane))
                  ) => Show (BoundedVertexF f plane)
deriving instance ( Eq plane
                  , Eq (NumType plane)
                  , Eq (f (LEEdge plane))
                  ) => Eq   (BoundedVertexF f plane)

-- instance Functor (BoundedVertex )



-- | Traverse the bounded vertex.
--
-- \(O(k\log k)\), where \(k\) is the number of definers of v.
traverseBoundedV   :: (Traversable f, Applicative g, NumType plane ~ NumType plane', Ord plane')
                   => (plane -> g plane') -> BoundedVertexF f plane -> g (BoundedVertexF f plane')
traverseBoundedV f = \case
  Vertex l defs es -> Vertex l <$> traverseSet f defs <*> traverse (traverse f) es

-- | Traverse on a Set.
traverseSet   :: (Ord b, Applicative f) => (a -> f b) -> Set.Set a -> f (Set.Set b)
traverseSet f = fmap Set.fromList . traverse f . Set.toAscList




-- | The location of the vertex
location :: (NumType plane ~ r) => Lens' (BoundedVertexF f plane) (Point 3 r)
location = lens _location (\v l -> v { _location = l })

-- | Projected 2d location of the point
location2 :: (NumType plane ~ r) => Getter (BoundedVertexF f plane) (Point 2 r)
location2 = location . to projectPoint

-- | The three planes defining the vertex
definers :: Lens' (BoundedVertexF f plane) (Set.Set plane)
definers = lens _definers (\v ds -> v { _definers = ds })

-- | Lens to access the incident edges of a vertex
incidentEdgesB :: Lens (BoundedVertexF f plane)
                       (BoundedVertexF g plane) (f (LEEdge plane)) (g (LEEdge plane))
incidentEdgesB = lens _incidentEdgesB (\v es -> v { _incidentEdgesB = es })

--------------------------------------------------------------------------------

-- | An (half)Edge in the Lower envelope
data LEEdge plane = Edge { _destination :: {-# UNPACK #-}!VertexID
                         , _leftPlane   :: plane
                         , _rightPlane  :: plane
                         } deriving stock (Show,Eq,Ord,Functor,Foldable,Traversable)

-- -- | Create an edge
-- createEdge         :: VertexID  -- ^ the destination
--                    -> plane     -- ^ the left plane
--                    -> plane     -- ^ the right plane
--                    -> LEEdge plane
-- createEdge u hl hr = Edge u hl hr

-- | Given some vertex u and an edge e from u towards some other
-- vertex v, flip the edge e, s othat it is the edge from v to u.
flipEdge                  :: VertexID -> LEEdge plane -> LEEdge plane
flipEdge u (Edge _ hl hr) = Edge u hr hl

-- | Getter to access the destination field of an edge.
destination :: Getter (LEEdge plane) VertexID
destination = to _destination


-- | Lens to access the plane left of/above the edge
leftPlane :: Lens' (LEEdge plane) plane
leftPlane = lens _leftPlane (\ed h -> ed { _leftPlane = h })

-- | Lens to access the plane right of/below the edge
rightPlane :: Lens' (LEEdge plane) plane
rightPlane = lens _rightPlane (\ed h -> ed { _rightPlane = h })

--------------------------------------------------------------------------------
