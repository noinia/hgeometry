{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Triangle
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data types to represent Triangles
--
--------------------------------------------------------------------------------
module HGeometry.Triangle
  ( Triangle(Triangle)
  , module HGeometry.Triangle.Class
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Triangle.Class
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | Triangles in d-dimensional space
newtype Triangle point = MkTriangle (Vector 3 point)

-- | Construct a triangle from its three points
pattern Triangle       :: OptCVector_ 3 point => point -> point -> point -> Triangle point
pattern Triangle a b c = MkTriangle (Vector3 a b c)

instance ( OptCVector_ 3 point
         , Point_ point (Dimension point) (NumType point)
         ) => Triangle_ (Triangle point) point where
  mkTriangle = Triangle
