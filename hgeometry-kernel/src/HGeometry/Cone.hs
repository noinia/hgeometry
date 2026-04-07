{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Cone
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Data type to represent Cones
--
--------------------------------------------------------------------------------
module HGeometry.Cone
  ( Cone(Cone), apex, leftBoundaryVector, rightBoundaryVector
  , leftBoundary, rightBoundary
  , coneBisector
  , intersectingHalfplanes
  ) where

import HGeometry.Vector
import HGeometry.Ext
import HGeometry.HalfLine
import HGeometry.Point
import HGeometry.HalfSpace
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.Properties
import Control.Lens
import Data.Bitraversable
import Data.Bifoldable

--------------------------------------------------------------------------------

-- | A Cone
data Cone r point edge = Cone { _apex                :: point
                              , _leftBoundaryVector  :: Vector 2 r :+ edge
                              -- ^ the interior of the cone is to the right
                              , _rightBoundaryVector :: Vector 2 r :+ edge
                              -- ^ the interior of the cone is to the left
                              }
                       deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

makeLenses ''Cone

type instance NumType   (Cone r point edge) = r
type instance Dimension (Cone r point edge) = 2


instance Bifunctor (Cone r) where
  bimap f g (Cone a l r) = Cone (f a) (l&extra %~ g) (r&extra %~ g)

instance Bifoldable (Cone r) where
  bifoldMap f g (Cone a l r) = f a <> g (l^.extra) <> g (r^.extra)

instance Bitraversable (Cone r) where
  bitraverse f g (Cone a l r) = Cone <$> f a <*> (l&extra %%~ g) <*> (r&extra %%~ g)


-- | Get the left boundary as a HalfLine starting at the apex.
leftBoundary   :: ( Dimension point ~ 2, NumType point ~ r)
               => Cone r point edge -> HalfLine point :+ edge
leftBoundary c = (c^.leftBoundaryVector)&core %~ HalfLine (c^.apex)

-- | Get the left boundary as a HalfLine starting at the apex.
rightBoundary   :: ( Dimension point ~ 2, NumType point ~ r)
                => Cone r point edge -> HalfLine point :+ edge
rightBoundary c = (c^.rightBoundaryVector)&core %~ HalfLine (c^.apex)


-- | Get the bisector of the cone
coneBisector   :: (Point_ point 2 r, Num r) => Cone r point edge -> HalfLine point
coneBisector c = HalfLine (c^.apex)
                         ((c^.leftBoundaryVector.core) ^+^ (c^.rightBoundaryVector.core))

-- | Get the two halfplanes so that the cone is the intersection of the two halfplanes.
-- the first halfplane is the plane right of the left boundary, whereas the
-- second halfplane is the plane left of the right boundary.
intersectingHalfplanes   :: ( Point_ point 2 r, Num r, Ord r)
                         => Cone r point edge -> Vector 2 (HalfSpaceF (LinePV 2 r))
intersectingHalfplanes c = Vector2 (rightHalfPlane $ LinePV a leftB)
                                   (leftHalfPlane  $ LinePV a rightB)
  where
    a      = c^.apex.asPoint
    leftB  = c^.leftBoundaryVector.core
    rightB = c^.rightBoundaryVector.core
{-# INLINE intersectingHalfplanes #-}

--------------------------------------------------------------------------------

instance ( Point_ point 2 r, Num r, Ord r
         ) => Point 2 r `HasIntersectionWith` Cone r point edge where
  q `intersects` cone = all (q `intersects`) (intersectingHalfplanes cone)
  {-# INLINE intersects #-}
