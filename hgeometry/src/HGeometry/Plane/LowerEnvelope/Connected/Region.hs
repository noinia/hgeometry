{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances  #-}
module HGeometry.Plane.LowerEnvelope.Connected.Region
  ( RegionF(..), Region, Prism
  , MDVertex(MDVertex), location, vertexData
  , ClippedBoundedRegion
  ) where

import HGeometry.Small.OneOrTwo
import Control.Lens hiding (Prism)
import Data.Bifoldable
import Data.List.NonEmpty (NonEmpty(..))
import HGeometry.Box
import HGeometry.Plane.LowerEnvelope.Connected.VertexForm (Definers, HasDefiners(..))
import HGeometry.Point
import HGeometry.Point.Either
import HGeometry.Polygon
import HGeometry.Polygon.Convex.Unbounded
import HGeometry.Properties
import HGeometry.Vector
import HGeometry.Cyclic
import HGeometry.Triangle
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Functor.Classes


--------------------------------------------------------------------------------

-- | A vertex of the minimzation Diagram that is defined by the intersection of a number
-- of planes.
--
-- Note that we interpet this vertex as a 2-dimensional thing.
data MDVertex r plane a  = MDVertex { _location   :: Point 3 r
                                    , _definers   :: Definers plane
                                     -- ^ the definers of the vertex
                                    , _vertexData :: a
                                    } deriving stock (Show,Eq,Ord,Functor,Foldable,Generic)
makeLenses ''MDVertex

type instance Dimension (MDVertex r plane a) = 2
type instance NumType   (MDVertex r plane a) = r

instance (NFData r, NFData plane, NFData a) => NFData (MDVertex r plane a)

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

type Prism r vertex = RegionF Triangle OneOrTwo


-- | Unbounded regions
type Region r vertex = RegionF (ConvexPolygonF (Cyclic NonEmpty)) NonEmpty r vertex

-- | A region in the minimization diagram. The boundary is given in CCW order; i.e. the
-- region is to the left of the boundary.
data RegionF bounded unbounded r vertex =
    BoundedRegion   (bounded vertex)
  | UnboundedRegion (UnboundedConvexRegionF r unbounded vertex)
   deriving stock (Functor,Foldable,Traversable,Generic)

type instance NumType   (RegionF bounded unbounded r point) = r
type instance Dimension (RegionF bounded unbounded r point) = Dimension point

instance (NFData r, NFData (unbounded vertex), NFData (bounded vertex)
         ) => NFData (RegionF bounded unbounded r vertex)

deriving instance ( Show r, Show vertex, Point_ vertex 2 r
                  , Show (bounded vertex), Show1 unbounded
                  ) => Show (RegionF bounded unbounded r vertex)

deriving instance (Eq r, Eq vertex, Eq (bounded vertex), Eq1 unbounded
                  ) => Eq (RegionF bounded unbounded r vertex)


--------------------------------------------------------------------------------
