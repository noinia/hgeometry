{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Matrix.ByRow
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- type-indexed matrices.
--
--------------------------------------------------------------------------------
module HGeometry.Matrix.ByRows(
    Matrix(Matrix)
  , OptMatrix_
  ) where

import           Control.Lens
-- import           GHC.TypeNats
import           HGeometry.Matrix.Class
import           HGeometry.Vector
import           HGeometry.Properties
-- import           Linear.Matrix (M22, M33, M44)
-- import qualified Linear.Matrix as Lin

--------------------------------------------------------------------------------
-- * Matrices

-- | A matrix of n rows, each of m columns, storing values of type r.
newtype Matrix n m r = Matrix (Vector n (Vector m r))


-- transpose :: Matrix n m r -> Matrix m n r
-- transpose = undefined


type instance NumType (Matrix n m r) = r
type instance Index   (Matrix n m r) = (Int,Int)
type instance IxValue (Matrix n m r) = r


_MatrixVector :: Iso (Matrix n m r)          (Matrix n m s)
                     (Vector n (Vector m r)) (Vector n (Vector m s))
_MatrixVector = iso (\(Matrix v) -> v) Matrix

deriving stock instance ( Show r
                        , Has_ Vector_ m r
                        , Has_ Vector_ n (Vector m r)
                        ) => Show (Matrix n m r)
deriving newtype instance ( Eq (Vector n (Vector m r)))  => Eq  (Matrix n m r)
deriving newtype instance ( Ord (Vector n (Vector m r))) => Ord (Matrix n m r)

-- | shorthand for square matrixecs
type OptMatrix_ d r = ( Has_ Additive_ d r
                      , Has_ Vector_ d (Vector d r)
                      , Ixed (Vector d r)
                      , Ixed (Vector d (Vector d r))
                      )

instance ( Has_ Vector_ n (Vector m r)
         , Has_ Vector_ m r
         , Ixed (Vector n (Vector m r))
         , Ixed (Vector m r)
         ) => Ixed (Matrix n m r) where
  ix (i,j) = _MatrixVector.ix i.ix j


instance ( Has_ Vector_ n (Vector m r)
         , Has_ Vector_ m r
         , Has_ Vector_ n (Vector m s)
         , Has_ Vector_ m s
         , HasComponents (Vector m r) (Vector m s)
         , HasComponents (Vector n (Vector m r)) (Vector n (Vector m s))
         ) => HasElements (Matrix n m r) (Matrix n m s) where
  elements = _MatrixVector .> components <.> components


instance ( Has_ Vector_ n (Vector m r)
         , Has_ Additive_ m r
         , Ixed (Vector n (Vector m r))
         , Ixed (Vector m r)
         ) => Matrix_ (Matrix n m r) n m r where

  matrixFromRows = Matrix . view _Vector

  generateMatrix f = Matrix $ generate mkRow
    where
      mkRow i = generate (\j -> f (i,j))

  rows = _MatrixVector

-- test :: Matrix 2 2 Int
-- test = identityMatrix

--  columns = undefined

-- instance Fractional r => Invertible 2 r where
--   -- >>> inverse' $ Matrix $ Vector2 (Vector2 1 2) (Vector2 3 4.0)
--   -- Matrix Vector2 [Vector2 [-2.0,1.0],Vector2 [1.5,-0.5]]
--   inverse' = withM22 Lin.inv22

-- instance Fractional r => Invertible 3 r where
--   -- >>> inverse' $ Matrix $ Vector3 (Vector3 1 2 4) (Vector3 4 2 2) (Vector3 1 1 1.0)
--   -- Matrix Vector3 [Vector3 [0.0,0.5,-1.0],Vector3 [-0.5,-0.75,3.5],Vector3 [0.5,0.25,-1.5]]
--   inverse' = withM33 Lin.inv33

-- instance Fractional r => Invertible 4 r where
--   inverse' = withM44 Lin.inv44

-- -- instance HasDeterminant 1 where
-- --   det (Matrix (Vector1 (Vector1 x))) = x
-- instance HasDeterminant 2 where
--   det = Lin.det22 . coerce
-- -- instance HasDeterminant 3 where
-- --   det = Lin.det33 . coerce
-- -- instance HasDeterminant 4 where
-- --   det = Lin.det44 . coerce

-- --------------------------------------------------------------------------------
-- -- Boilerplate code for converting between Matrix and M22/M33/M44.

-- withM22   :: (M22 a -> M22 b) -> Matrix 2 2 a -> Matrix 2 2 b
-- withM22 f = coerce . f . coerce

-- withM33 :: (M33 a -> M33 b) -> Matrix 3 3 a -> Matrix 3 3 b
-- withM33 f = coerce . f . coerce

-- withM44 :: (M44 a -> M44 b) -> Matrix 4 4 a -> Matrix 4 4 b
-- withM44 f = coerce . f . coerce
