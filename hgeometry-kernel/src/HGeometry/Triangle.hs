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
import HGeometry.Box.Boxable
-- import HGeometry.HalfSpace
-- import HGeometry.HyperPlane
import HGeometry.Point
import HGeometry.Properties
-- import HGeometry.Transformation
import HGeometry.Triangle.Class
import HGeometry.Vector
import Text.Read

--------------------------------------------------------------------------------

-- | Triangles in d-dimensional space
newtype Triangle point = MkTriangle (Vector 3 point)

-- | Construct a triangle from its three points
pattern Triangle       :: point -> point -> point -> Triangle point
pattern Triangle a b c = MkTriangle (Vector3 a b c)
{-# COMPLETE Triangle #-}


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

instance HasPoints (Triangle point) (Triangle point') point point' where
  allPoints = _TriangleVector.components

-- instance ( DefaultTransformByConstraints (Triangle point) d r
--          , Point_ point d r
--          , d > 0
--          ) => IsTransformable (Triangle point)


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

-- instance ( Point_ point 2 r
--          , Num r, Ord r
--          , OptCVector_ 3 (HalfSpace 2 r)
--          , OptMetric_ 2 r, OptCVector_ 2 r
--          , OptCVector_ 3 r
--          , OptCVector_ 3 point
--          -- , HyperPlane_ (HyperPlane 2 r) 2 r
--          ) => HasIntersectionWith (Point 2 r) (Triangle point) where
--   q `intersects` t = allOf components (q `intersects`) $ intersectingHalfPlanes t
