{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances  #-}
module HGeometry.Plane.LowerEnvelope.Connected.Region
  ( Region(..)
  , MDVertex(MDVertex), location
  , ClippedBoundedRegion
  ) where

import           Control.Lens
import           Data.List.NonEmpty (NonEmpty(..))
-- import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.HyperPlane
import           HGeometry.Intersection
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope.Connected.VertexForm (Definers)
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Convex.Unbounded
import           HGeometry.Polygon.Simple
import           HGeometry.Properties
import           HGeometry.Sequence.Alternating (separators)
import           HGeometry.Triangle
import           HGeometry.Vector

import           HGeometry.Cyclic

--------------------------------------------------------------------------------

-- | A vertex of the minimzation Diagram that is defined by the intersection of a number
-- of planes.
--
-- Note that we interpet this vertex as a 2-dimensional thing.
data MDVertex r plane = MDVertex { _location :: Point 3 r
                                 , _definers :: Definers plane
                                 -- ^ the definers of the vertex
                                 } deriving (Show,Eq,Functor,Foldable)
makeLenses ''MDVertex

type instance Dimension (MDVertex r plane) = 2
type instance NumType   (MDVertex r plane) = r

instance Affine_ (MDVertex r plane) 2 r where
instance HasVector (MDVertex r plane) (MDVertex r plane) where
  vector = lens (^.location.vector.to prefix)
                (\v (Vector2 x y) -> v&location %~ \(Point3 _ _ z) -> Point3 x y z)

instance HasCoordinates (MDVertex r plane) (MDVertex r plane) where
  -- ^ Note that this only traverses the x and y coordinates of the vertex!
  coordinates = vector.traversed1

instance Num r => Point_ (MDVertex r plane) 2 r where

--------------------------------------------------------------------------------

-- | A Convex bounded region, which may be clipped using vertices of type 'corner'.
type ClippedBoundedRegion r vertex corner =
  ConvexPolygonF NonEmpty (OriginalOrExtra vertex corner)

--------------------------------------------------------------------------------

-- | A region in the minimization diagram. The boundary is given in CCW order; i.e. the
-- region is to the left of the boundary.
data Region r vertex = BoundedRegion   (ConvexPolygonF NonEmpty vertex)
                     | UnboundedRegion (UnboundedConvexRegionF r NonEmpty vertex)
                     deriving stock (Functor,Foldable,Traversable)

type instance NumType   (Region r point) = r
type instance Dimension (Region r point) = Dimension point

deriving instance (Show r, Show vertex, Point_ vertex 2 r) => Show (Region r vertex)
deriving instance (Eq r, Eq vertex) => Eq (Region r vertex)
