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
  ( Slab(..)
  ) where

import HGeometry.Properties (NumType,Dimension)

--------------------------------------------------------------------------------

data Slab r orientedLine = Slab { _definingLine        :: orientedLine
                                , _signedSquaredWidth  :: !r
                                }
                         deriving stock (Show,Eq,Ord,Functor,Foldable)

type instance NumType   (Slab r orientedLine) = r
type instance Dimension (Slab r orientedLine) = 2
