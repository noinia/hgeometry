{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.HalfSpace
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Representing halfspaces
--
--------------------------------------------------------------------------------
module HGeometry.HalfSpace
  ( HalfSpaceF(..)
  , halfspaceSign, boundingHyperPlane
  , HalfSpace
  , Sign(..)
  ) where

import Control.Lens
import HGeometry.HyperPlane
import HGeometry.Intersection
import HGeometry.Point
import HGeometry.Properties (NumType,Dimension)
import HGeometry.Sign
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | A HalfSpace bounded by a hyperplane. The sign indicates which
-- side of the bounding hyperplane is indicated. Iff the sign is
-- positive, we mean the points for which onSideTest returns a
-- positive value.
--
-- Half spaces include their bounding hyperplane.
data HalfSpaceF boundingHyperPlane =
    HalfSpace {-# UNPACK #-} !Sign boundingHyperPlane
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

-- | Lens to access the sign of the halfspace
halfspaceSign :: Lens' (HalfSpaceF boundingHyperPlane) Sign
halfspaceSign = lens (\(HalfSpace s _) -> s) (\(HalfSpace _ h) s -> HalfSpace s h)
{-# INLINE halfspaceSign #-}

-- | Lens to access the hyperplane bounding the halfspace
boundingHyperPlane :: Lens (HalfSpaceF boundingHyperPlane) (HalfSpaceF boundingHyperPlane')
                           boundingHyperPlane              boundingHyperPlane'
boundingHyperPlane = lens (\(HalfSpace _ h) -> h) (\(HalfSpace s _) h -> HalfSpace s h)
{-# INLINE boundingHyperPlane #-}


-- | Arbitrary halfspaces in r^d.
type HalfSpace d r = HalfSpaceF (HyperPlane d r)

type instance Dimension (HalfSpaceF boundingHyperPlane) = Dimension boundingHyperPlane
type instance NumType   (HalfSpaceF boundingHyperPlane) = NumType boundingHyperPlane

instance ( HyperPlane_ boudingHyperPlane d r, Ord r, Num r
         , Has_ Additive_ d r
         ) => HasIntersectionWith (Point d r) (HalfSpaceF boudingHyperPlane) where
  q `intersects` (HalfSpace s h) = case s of
                                     Positive -> q `onSideTest` h /= LT
                                     Negative -> q `onSideTest` h /= GT

type instance Intersection (Point d r) (HalfSpaceF boudingHyperPlane) = Maybe (Point d r)

instance ( HyperPlane_ boudingHyperPlane d r, Ord r, Num r
         , Has_ Additive_ d r
         ) => IsIntersectableWith (Point d r) (HalfSpaceF boudingHyperPlane) where
  q `intersect` h
    | q `intersects` h = Just q
    | otherwise        = Nothing

-- type IntersectionOf (LinePV d r) (HalfSpaceF boudingHyperPlane) = Maybe (Point d r)


--------------------------------------------------------------------------------
