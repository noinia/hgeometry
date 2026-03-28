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
  , OptMatrix_
  , module HGeometry.Matrix.Class
  ) where


import HGeometry.Matrix.ByRows
import HGeometry.Matrix.Class


-- import           GHC.TypeNats
-- import           HGeometry.Vector
-- import           Data.Maybe (fromMaybe)


-- matrixFromList' :: (Matrix_ matrix n m r
--                    , KnownNat n, KnownNat m
--                    , Has_ Vector_  m r
--                    , Has_ Vector_  n (Vector m r)
--                    ) => [r] -> matrix
-- matrixFromList' = fromMaybe (error "") . matrixFromList
