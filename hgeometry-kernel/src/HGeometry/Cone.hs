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
  ) where

import HGeometry.Vector
import HGeometry.Ext
import HGeometry.HalfLine
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
