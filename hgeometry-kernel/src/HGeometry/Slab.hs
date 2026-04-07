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
  ( Slab(Slab), definingLine, signedSquaredWidth
  ) where

import HGeometry.Properties (NumType,Dimension)
import Control.Lens

--------------------------------------------------------------------------------

-- | A data type representing a slab.
data Slab r orientedLine = Slab { _definingLine        :: orientedLine
                                  -- ^ The oriented line that defines the slab. This
                                  -- is the left bounding line of the slab; i.e.
                                  -- the interior of the slab is to its right
                                , _signedSquaredWidth  :: !r
                                -- ^ the
                                }
                         deriving stock (Show,Eq,Ord,Functor,Foldable)

makeLenses ''Slab

type instance NumType   (Slab r orientedLine) = r
type instance Dimension (Slab r orientedLine) = 2
