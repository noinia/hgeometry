{-# LANGUAGE UndecidableInstances #-}
module HGeometry.HalfSpace
  ( HalfSpaceF(..)
  , HalfSpace
  ) where

import HGeometry.HyperPlane
import HGeometry.Intersection
import HGeometry.Point
import HGeometry.Properties (NumType,Dimension)
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | A HalfSpace bounded by a hyperplane.
--
-- i.e. the points on or above the given hyperplane.
newtype HalfSpaceF boundingHyperPlane = HalfSpace boundingHyperPlane
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

-- | Arbitrary halfspaces in r^d.
type HalfSpace d r = HalfSpaceF (HyperPlane d r)

type instance Dimension (HalfSpaceF boundingHyperPlane) = Dimension boundingHyperPlane
type instance NumType   (HalfSpaceF boundingHyperPlane) = NumType boundingHyperPlane


instance ( HyperPlane_ boudingHyperPlane d r, Ord r, Num r
         , Has_ Additive_ d r
         ) => HasIntersectionWith (Point d r) (HalfSpaceF boudingHyperPlane) where
  q `intersects` (HalfSpace h) = q `onSideTest` h /= LT
