module HGeometry.Matrix.Class
  ( Matrix_(..)
  , HasElements(..)
  , HasDeterminant(..)
  , Invertible(..)
  ) where

import Control.Lens hiding (cons,snoc,uncons,unsnoc)
import Data.Kind
import GHC.TypeNats
import HGeometry.Properties
import HGeometry.Vector
import HGeometry.Vector.List
import Prelude hiding (zipWith)


--------------------------------------------------------------------------------

class HasElements matrix matrix' where
  -- | IndexedTraversal over the elements of the matrix, each index is
  -- a (row,column) index pair.
  elements :: IndexedTraversal (Int,Int) matrix matrix' (NumType matrix) (NumType matrix')

-- | A matrix of n rows, each of m columns, storing values of type r.
type Matrix_ :: Type -> Nat -> Nat -> Type -> Constraint
class ( r ~ NumType matrix
      , Ixed matrix
      , IxValue matrix ~ r
      , Index matrix ~ (Int,Int) -- ^ row, col
      , HasElements matrix matrix
      ) => Matrix_ matrix n m r | matrix -> n
                                , matrix -> m
                                , matrix -> r where
  -- | Produces the Identity Matrix.
  identityMatrix :: Num r => matrix

  -- | Matrix multiplication
  (!*!)     :: ( Matrix_ matrix'  m m' r
               , Matrix_ matrix'' n m' r
               , Num r
               ) => matrix -> matrix' -> matrix''
  -- ma !*! mb = generateMatrix $ \(i,j) -> row i ma `dot` column j mb

  -- ma !*! mb = fmap (\ f' -> Foldable.foldl' (^+^) zero $ liftI2 (*^) f' mb) ma

  -- | Multiply a matrix and a vector.
  (!*)  :: ( Vector_ vector n r, Num r
           , OptVector_ m r
           ) => matrix -> vector -> vector
  m !* v = vZipWith f (ListVector @n $ m^..rows) v
    where
      f       :: Vector m r -> r -> r
      f row x = sumOf components $ x *^ row

  -- | traversal over all rows
  rows :: IndexedTraversal' Int matrix (Vector m r)

  -- | Traversal over all columns
  columns :: IndexedTraversal' Int matrix (Vector n r)

  {-# MINIMAL identityMatrix, (!*!), rows, columns #-}

infixl 7 !*!
infixl 7 !*

-- | Dimensions for which we can compute the determinant of a matrix
class HasDeterminant d where
  det :: (Num r, Matrix_ matrix d d r) => matrix -> r

-- | Class of matrices that are invertible.
class Invertible n r where
  inverse' :: (Matrix_ matrix n n r) => matrix -> matrix



-- -- | Creates a row with zeroes everywhere, except at position i, where the
-- -- value is the supplied value.
-- mkRow     :: forall vector d r. (Vector_ vector d r, Num r) => Int -> r -> vector
-- mkRow i x = set (unsafeComponent i) x zero


--------------------------------------------------------------------------------
