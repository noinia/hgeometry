{-# LANGUAGE UndecidableInstances #-}
module HGeometry.LowerEnvelope.Type
  ( VertexID
  , BoundedVertexF(Vertex), _incidentEdgesB
  , location, definers, location2

  , LEEdge(Edge)
  ) where

import           Control.Lens
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           HGeometry.Foldable.Sort
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line
import           HGeometry.Line.LineEQ
import           HGeometry.Point
import           HGeometry.Properties
import           Hiraffe.Graph

--------------------------------------------------------------------------------

-- | VertexIds are just ints
type VertexID = Int

-- | Orders the plane around their intersection point.
newtype AroundVertex plane = AroundVertex plane
                           deriving (Show,Eq)

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

-- | The location of the vertex
location :: (NumType plane ~ r) => Lens' (BoundedVertexF f plane) (Point 3 r)
location = lens _location (\v l -> v { _location = l })

-- | Projected 2d location of the point
location2 :: (NumType plane ~ r) => Getter (BoundedVertexF f plane) (Point 2 r)
location2 = location . to projectPoint

-- | The three planes defining the vertex
definers :: Lens' (BoundedVertexF f plane) (Set.Set plane)
definers = lens _definers (\v ds -> v { _definers = ds })

--------------------------------------------------------------------------------

-- | An Edge in the Lower envelope
data LEEdge plane = Edge { _destination :: {-# UNPACK #-}!VertexID
                         , _leftPlane   :: plane
                         , _rightPlane  :: plane
                         } deriving (Show,Eq,Ord)
