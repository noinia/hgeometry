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
  , HalfSpace
  , Sign(..)
  , module HGeometry.HalfSpace.Class

  , Point_x_HalfSpace_Intersection(..)


  , boundingHyperPlaneLens
  ) where

import Control.Lens
import Data.Bifoldable
import Data.Bifoldable1
import Data.Bitraversable
import Data.Semigroup.Bitraversable
import HGeometry.HalfSpace.Class
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

instance ( NumType boundingHyperPlane ~ r
         , Dimension boundingHyperPlane ~ d
         ) => HalfSpace_ (HalfSpaceF boundingHyperPlane) d r where
  type BoundingHyperPlane (HalfSpaceF boundingHyperPlane) d r = boundingHyperPlane

  halfSpaceSign = lens (\(HalfSpace s _) -> s) (\(HalfSpace _ h) s -> HalfSpace s h)
  {-# INLINE halfSpaceSign #-}
  boundingHyperPlane = boundingHyperPlaneLens

-- | Lens to access the hyperplane bounding the halfspace
--
-- Generally, one can just use boundingHyperPlane from the HalfSpace_ class. But this one
-- is useful if you need a lens to change the type of the bounding hyperplane.
boundingHyperPlaneLens :: Lens (HalfSpaceF boundingHyperPlane) (HalfSpaceF boundingHyperPlane')
                           boundingHyperPlane              boundingHyperPlane'
boundingHyperPlaneLens = lens (\(HalfSpace _ h) -> h) (\(HalfSpace s _) h -> HalfSpace s h)
{-# INLINE boundingHyperPlaneLens #-}

-- | Arbitrary halfspaces in r^d.
type HalfSpace d r = HalfSpaceF (HyperPlane d r)

type instance Dimension (HalfSpaceF boundingHyperPlane) = Dimension boundingHyperPlane
type instance NumType   (HalfSpaceF boundingHyperPlane) = NumType boundingHyperPlane

instance ( HyperPlane_ boudingHyperPlane d r, Ord r, Num r
         , Has_ Additive_ d r
         ) => HasIntersectionWith (Point d r) (HalfSpaceF boudingHyperPlane)

type instance Intersection (Point d r) (HalfSpaceF boudingHyperPlane) =
  Maybe (Point_x_HalfSpace_Intersection (Point d r) (Point d r))

-- | Point x HalfSpace intersection
data Point_x_HalfSpace_Intersection boundaryPoint interiorPoint =
    Point_x_HalfSpace_OnBoundary boundaryPoint
  | Point_x_HalfSpace_Interior   interiorPoint
  deriving (Show,Eq,Functor,Foldable,Traversable)

instance Bifunctor Point_x_HalfSpace_Intersection where
  bimap f g = \case
    Point_x_HalfSpace_OnBoundary p -> Point_x_HalfSpace_OnBoundary (f p)
    Point_x_HalfSpace_Interior p   -> Point_x_HalfSpace_Interior (g p)

instance Bifoldable Point_x_HalfSpace_Intersection where
  bifoldMap = bifoldMap1

instance Bifoldable1 Point_x_HalfSpace_Intersection where
  bifoldMap1 f g = \case
    Point_x_HalfSpace_OnBoundary p -> f p
    Point_x_HalfSpace_Interior p   -> g p

instance Bitraversable Point_x_HalfSpace_Intersection where
  bitraverse f g = \case
    Point_x_HalfSpace_OnBoundary p -> Point_x_HalfSpace_OnBoundary <$> f p
    Point_x_HalfSpace_Interior p   -> Point_x_HalfSpace_Interior   <$> g p

instance Bitraversable1 Point_x_HalfSpace_Intersection where
  bitraverse1 f g = \case
    Point_x_HalfSpace_OnBoundary p -> Point_x_HalfSpace_OnBoundary <$> f p
    Point_x_HalfSpace_Interior p   -> Point_x_HalfSpace_Interior   <$> g p



instance ( HyperPlane_ boudingHyperPlane d r, Ord r, Num r
         , Has_ Additive_ d r
         ) => IsIntersectableWith (Point d r) (HalfSpaceF boudingHyperPlane) where
  q `intersect` (HalfSpace s h) = case s of
    Positive -> case q `onSideTest` h of
                  LT -> Nothing
                  EQ -> Just $ Point_x_HalfSpace_OnBoundary q
                  GT -> Just $ Point_x_HalfSpace_Interior q
    Negative -> case q `onSideTest` h of
                  LT -> Just $ Point_x_HalfSpace_Interior q
                  EQ -> Just $ Point_x_HalfSpace_OnBoundary q
                  GT -> Nothing

instance ( HasSquaredEuclideanDistance boundingHyperPlane
         , HasIntersectionWith (Point d r) (HalfSpaceF boundingHyperPlane)
         , d ~ Dimension boundingHyperPlane, r ~ NumType boundingHyperPlane
         )
         => HasSquaredEuclideanDistance (HalfSpaceF boundingHyperPlane) where
  pointClosestTo (view asPoint -> q) h
    | q `intersects` h = q
    | otherwise        = pointClosestTo q (h^.boundingHyperPlane)


--------------------------------------------------------------------------------


instance (HasIntersectionWith line line'
         , HyperPlane_ line 2 r, HyperPlane_ line' 2 r
         , Ord r, Fractional r
         )
       => HasIntersectionWith (HalfSpaceF line) (HalfSpaceF line') where
  h@(HalfSpace _ l) `intersects` h'@(HalfSpace _ l') =
    l `intersects` l' || pointOn l `intersects`  h' || pointOn l' `intersects`  h
