--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Triangle.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A class of types representing Triangles
--
--------------------------------------------------------------------------------
module HGeometry.Triangle.Class
  ( Triangle_(..)
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           HGeometry.Point.Class
import           HGeometry.Vector
import           HGeometry.Properties

--------------------------------------------------------------------------------

-- | Class representing triangles
class ( Point_   point (Dimension point) (NumType point)
      )
     => Triangle_ triangle point | triangle -> point where

  -- | Construct a triangle from its three vertices.
  mkTriangle :: point -> point -> point -> triangle
