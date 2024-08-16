{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Triangle
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data types to represent Triangles
--
--------------------------------------------------------------------------------
module HGeometry.Triangle
  ( Triangle(Triangle)
  , module HGeometry.Triangle.Class
  ) where

import Control.Lens
import Data.Foldable1
import GHC.Generics (Generic)
import HGeometry.Box.Boxable
import HGeometry.Intersection
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Transformation
import HGeometry.Triangle.Class
import HGeometry.Vector
import Hiraffe.Graph
import Text.Read

--------------------------------------------------------------------------------

-- | Triangles in d-dimensional space
newtype Triangle point = MkTriangle (Vector 3 point)
  deriving (Generic)
  deriving newtype (Functor,Foldable)

-- | Construct a triangle from its three points
pattern Triangle       :: point -> point -> point -> Triangle point
pattern Triangle a b c = MkTriangle (Vector3 a b c)
{-# COMPLETE Triangle #-}

instance Traversable Triangle where
  traverse f (MkTriangle v) = MkTriangle <$> traverse f v

instance Foldable1 Triangle where
  foldMap1 f (MkTriangle v) = foldMap1 f v

instance Traversable1 Triangle where
  traverse1 f (MkTriangle v) = MkTriangle <$> traverse1 f v


deriving instance Eq  (Vector 3 point) => Eq (Triangle point)
deriving instance Ord (Vector 3 point) => Ord (Triangle point)

type instance Dimension (Triangle point) = Dimension point
type instance NumType   (Triangle point) = NumType point

-- | Iso between a triangle and a vector of three points
_TriangleVector :: Iso (Triangle point) (Triangle point') (Vector 3 point) (Vector 3 point')
_TriangleVector = iso (\(MkTriangle v) -> v) MkTriangle

instance ( Point_ point (Dimension point) (NumType point)
         ) => Triangle_ (Triangle point) point where
  mkTriangle = Triangle
  corners = _TriangleVector

instance HasVertices' (Triangle point) where
  type Vertex   (Triangle point) = point
  type VertexIx (Triangle point) = Int
  vertexAt i = _TriangleVector.iix i

instance HasVertices (Triangle point) (Triangle point') where
  vertices = cloneIndexedTraversal1 (_TriangleVector.components)

instance HasPoints (Triangle point) (Triangle point') point point' where
  allPoints = _TriangleVector.components

instance ( DefaultTransformByConstraints (Triangle point) d r
         , Point_ point d r
         ) => IsTransformable (Triangle point)

instance (Show point) => Show (Triangle point) where
  showsPrec k (Triangle a b c ) = showParen (k > appPrec) $
                                    showString "Triangle "
                                    . showsPrec (appPrec+1) a
                                    . showChar ' '
                                    . showsPrec (appPrec+1) b
                                    . showChar ' '
                                    . showsPrec (appPrec+1) c

appPrec :: Int
appPrec = 10

instance (Read point) => Read (Triangle point) where
  readPrec = parens (prec appPrec $ do
                          Ident "Triangle" <- lexP
                          b <- step readPrec
                          a <- step readPrec
                          c <- step readPrec
                          return (Triangle a b c))

instance ( Point_ point d r
         , Ord (Vector d r)
         ) => IsBoxable (Triangle point)

instance ( Point_ point 2 r
         , Num r, Ord r
         ) => HasIntersectionWith (Point 2 r) (Triangle point) where
  q `intersects` t = allOf components (q `intersects`) $ intersectingHalfPlanes t
