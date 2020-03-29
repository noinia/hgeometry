{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Box
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Orthogonal \(d\)-dimensiontal boxes (e.g. rectangles)
--
--------------------------------------------------------------------------------
module Data.Geometry.Box( module Data.Geometry.Box.Internal
                        , module Data.Geometry.Box.Corners
                        , module Data.Geometry.Box.Sides
                        ) where

import Control.DeepSeq
import Control.Lens (makeLenses)
import Data.Functor.Apply
import Data.Geometry.Box.Corners
import Data.Geometry.Box.Internal
import Data.Geometry.Box.Sides
import Data.Geometry.LineSegment
import Data.Geometry.Vector
import Data.Semigroup.Foldable.Class
import Data.Semigroup.Traversable.Class
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

deriving instance (NFData p, NFData r, Arity d) => NFData (Box d p r)
