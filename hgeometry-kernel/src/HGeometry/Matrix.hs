--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Matrix
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- type-indexed matrices.
--
--------------------------------------------------------------------------------
module HGeometry.Matrix(
    Matrix(Matrix)
  , module HGeometry.Matrix.Class
  ) where


import HGeometry.Matrix.ByRows(Matrix(Matrix))
import HGeometry.Matrix.Class
