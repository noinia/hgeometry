{-# LANGUAGE TemplateHaskell  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Slab
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A data type to represent slabs; i.e. regions bounded by two parallel lines.
--
--------------------------------------------------------------------------------
module HGeometry.Slab
  ( Slab(Slab), definingLine, squaredWidth, leftData, rightData
  ) where

import HGeometry.Properties (NumType,Dimension)
import HGeometry.Line.PointAndVector
import Control.Lens

--------------------------------------------------------------------------------

-- | A data type representing a slab.
data Slab r side = Slab { _definingLine        :: !(LinePV 2 r)
                        -- ^ The oriented line that defines the slab. This
                        -- is the left bounding line of the slab; i.e.
                        -- the interior of the slab is to its right
                        , _squaredWidth  :: !r
                        -- ^ the squared width of the slab
                        , _leftData  :: side
                        -- ^ data associated with the left bounding line
                        , _rightData :: side
                        -- ^ data associated with the right bounding line
                        }
                 deriving stock (Show,Eq,Functor,Foldable)

makeLenses ''Slab

type instance NumType   (Slab r side) = r
type instance Dimension (Slab r side) = 2
