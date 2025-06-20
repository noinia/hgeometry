{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances  #-}
module HGeometry.Plane.LowerEnvelope.Connected.Region
  ( Region(..)
  , MDVertex(MDVertex), location, vertexData
  , ClippedBoundedRegion
  ) where

import Control.Lens
import Data.Bifoldable
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty(..))
import HGeometry.Box
import HGeometry.Ext
import HGeometry.HyperPlane
import HGeometry.Intersection
import HGeometry.Number.Real.Rational
import HGeometry.Plane.LowerEnvelope.Connected.VertexForm (Definers, HasDefiners(..))
import HGeometry.Point
import HGeometry.Point.Either
import HGeometry.Polygon
import HGeometry.Polygon.Convex
import HGeometry.Polygon.Convex.Unbounded
import HGeometry.Polygon.Simple
import HGeometry.Properties
import HGeometry.Sequence.Alternating (separators)
import HGeometry.Triangle
import HGeometry.Vector

import HGeometry.Cyclic

--------------------------------------------------------------------------------

-- | A vertex of the minimzation Diagram that is defined by the intersection of a number
-- of planes.
--
-- Note that we interpet this vertex as a 2-dimensional thing.
data MDVertex r plane a  = MDVertex { _location   :: Point 3 r
                                    , _definers   :: Definers plane
                                     -- ^ the definers of the vertex
                                    , _vertexData :: a
                                    } deriving (Show,Eq,Ord,Functor,Foldable)
makeLenses ''MDVertex

type instance Dimension (MDVertex r plane a) = 2
type instance NumType   (MDVertex r plane a) = r

instance Num r => IsBoxable (MDVertex r plane a) where
  boundingBox = boundingBox . view asPoint

instance Bifunctor (MDVertex r) where
  bimap f g (MDVertex l defs x) = MDVertex l (f <$> defs) (g x)
instance Bifoldable (MDVertex r) where
  bifoldMap f g (MDVertex _ defs x)  = foldMap f defs <> g x


instance Affine_ (MDVertex r plane a) 2 r where
instance HasVector (MDVertex r plane a) (MDVertex r plane a) where
  vector = lens (^.location.vector.to prefix)
                (\v (Vector2 x y) -> v&location %~ \(Point3 _ _ z) -> Point3 x y z)

instance HasDefiners (MDVertex r plane a) plane where
  definersOf = view definers

instance HasCoordinates (MDVertex r plane a) (MDVertex r plane a) where
  -- ^ Note that this only traverses the x and y coordinates of the vertex!
  coordinates = vector.traversed1

instance Num r => Point_ (MDVertex r plane a) 2 r where

--------------------------------------------------------------------------------

-- | A Convex bounded region, which may be clipped using vertices of type 'corner'.
type ClippedBoundedRegion r vertex corner =
  ConvexPolygonF (Cyclic NonEmpty) (OriginalOrExtra vertex corner)

--------------------------------------------------------------------------------

-- | A region in the minimization diagram. The boundary is given in CCW order; i.e. the
-- region is to the left of the boundary.
data Region r vertex = BoundedRegion   (ConvexPolygonF (Cyclic NonEmpty) vertex)
                     | UnboundedRegion (UnboundedConvexRegionF r NonEmpty vertex)
                     deriving stock (Functor,Foldable,Traversable)

type instance NumType   (Region r point) = r
type instance Dimension (Region r point) = Dimension point

deriving instance (Show r, Show vertex, Point_ vertex 2 r) => Show (Region r vertex)
deriving instance (Eq r, Eq vertex) => Eq (Region r vertex)


--------------------------------------------------------------------------------
