{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.HyperPlane.Intersection
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Intersection between hyperplanes in R^3
--
--------------------------------------------------------------------------------
module HGeometry.HyperPlane.Intersection
  ( PlanePlaneIntersection(..)
  , planePlaneIntersection
  ) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Type.Ord
import GHC.TypeNats
import HGeometry.HyperPlane.Class
import HGeometry.HyperPlane.Internal
import HGeometry.HyperPlane.NonVertical
import HGeometry.Intersection
import HGeometry.Line.General
import HGeometry.Line.LineEQ
import HGeometry.Vector

--------------------------------------------------------------------------------

type instance Intersection (HyperPlane 3 r) (HyperPlane 3 r) =
  Maybe (PlanePlaneIntersection (Plane r) (VerticalOrLineEQ r))

type instance Intersection (Plane r) (Plane r) =
  Maybe (PlanePlaneIntersection (Plane r) (VerticalOrLineEQ r))

-- | The intersection between two planes in R^3.
data PlanePlaneIntersection plane line = Plane_x_Plane_Line line
                                       | Plane_x_Plane_Plane plane
                                       deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

instance Bifunctor PlanePlaneIntersection where
  bimap f g = \case
    Plane_x_Plane_Line  l -> Plane_x_Plane_Line $ g l
    Plane_x_Plane_Plane h -> Plane_x_Plane_Plane $ f h

instance Bifoldable PlanePlaneIntersection where
  bifoldMap f g = \case
    Plane_x_Plane_Line  l -> g l
    Plane_x_Plane_Plane h -> f h

instance Bitraversable PlanePlaneIntersection where
  bitraverse f g = \case
    Plane_x_Plane_Line  l -> Plane_x_Plane_Line <$> g l
    Plane_x_Plane_Plane h -> Plane_x_Plane_Plane <$> f h

instance ( Has_ Metric_ d r, Num r, Eq r, 2 <= d, d < d + 1 -- this constraint is rather silly
         , Has_ Metric_ (1+d) r, Eq (Vector (1 + d) r)
         ) => HasIntersectionWith (HyperPlane d r) (HyperPlane d r) where
  h `intersects` h' = h == h' || not (h `isParallelTo` h')

instance ( Has_ Metric_ d r, Num r, Eq r, 2 <= d, d < d + 1
         , Has_ Metric_ (1+d) r, Has_ Additive_ (d-1) r, Eq (Vector d r)
         ) => HasIntersectionWith (NonVerticalHyperPlane d r) (NonVerticalHyperPlane d r) where
  h `intersects` h' = h == h' ||  not (h `isParallelTo` h')


instance (Eq r, Fractional r)
         => IsIntersectableWith (Plane r) (Plane r) where
  intersect = planePlaneIntersection


-- | Given two planes, computes the line in which they intersect.
planePlaneIntersection :: (Plane_ plane r, Fractional r, Eq r)
                       => plane -> plane
                       -> Maybe (PlanePlaneIntersection plane (VerticalOrLineEQ r))
planePlaneIntersection h@(Plane_ a1 b1 c1) (Plane_ a2 b2 c2)
    | b1 /= b2  = Just . Plane_x_Plane_Line . NonVertical
                $ LineEQ ((a2 - a1) / diffB) ((c2 - c1) / diffB)
                  -- the two planes intersect in some normal line
    | a1 /= a2  = Just . Plane_x_Plane_Line
                $ VerticalLineThrough ((c2 -c1) / (a1 - a2))
                  -- the planes intersect in a vertical line
    | c1 == c2  = Just $ Plane_x_Plane_Plane h
                  -- all others are the same
    | otherwise = Nothing
                  -- the planes don't intersect at all
  where
    diffB = b1 - b2
