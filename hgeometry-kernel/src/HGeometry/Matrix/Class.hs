--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Matrix.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A class of types representing matrices
--
--------------------------------------------------------------------------------
module HGeometry.Matrix.Class
  ( Matrix_(..)
  , HasElements(..)
  , HasDeterminant(..)
  , Invertible(..)
  ) where

import           Control.Lens hiding (cons,snoc,uncons,unsnoc,elements)
import           Data.Kind
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           GHC.TypeNats
import           HGeometry.Properties
import           HGeometry.Vector
import           HGeometry.Vector.List (ListVector(..))
import           Prelude hiding (zipWith)

--------------------------------------------------------------------------------
-- $setup
-- >>> import HGeometry.Point
-- >>> import HGeometry.Matrix

-- | Types that have an 'elements' field lens.
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
  {-# MINIMAL generateMatrix, matrixFromRows, rows #-}

  -- | Produces the Identity Matrix.
  --
  -- >>> mapMOf_ rows print $ identityMatrix @(Matrix 3 3 Int)
  -- Vector3 1 0 0
  -- Vector3 0 1 0
  -- Vector3 0 0 1
  identityMatrix :: Num r => matrix
  identityMatrix = generateMatrix $ \(i,j) -> if i == j then 1 else 0

  -- | Given a function that specifies the values, generate the matrix
  generateMatrix :: ((Int,Int) -> r) -> matrix

  -- | Given a list of the elements in the matrix, in row by row
  -- order, constructs the matrix.
  -- requires that there are exactly n*m elements.
  --
  -- >>> matrixFromList @(Matrix 2 3 Int) [1,2,3,4,5,6]
  -- Just (Matrix (Vector2 (Vector3 1 2 3) (Vector3 4 5 6)))
  -- >>> matrixFromList @(Matrix 2 3 Int) [1,2,3,4,5,6,7]
  -- Nothing
  matrixFromList    :: ( KnownNat n, KnownNat m
                       , OptVector_  m r
                       ) => [r] -> Maybe matrix
  matrixFromList xs = do rs  <- go n xs
                         rs' <- vectorFromList @(ListVector n (Vector m r)) rs
                         pure $ matrixFromRows rs'
    where
      m = fromIntegral $ natVal $ Proxy @m
      n = fromIntegral $ natVal $ Proxy @n

      go 0  [] = Just []
      go 0  _  = Nothing
      go n' ys = let (r,rest) = List.splitAt m ys
                 in do r'    <- vectorFromList @(Vector m r) r
                       rest' <- go (n'-1) rest
                       pure (r':rest')

  -- | Given a list of the elements in the matrix, in row by row
  -- order, constructs the matrix.
  -- requires that there are exactly n*m elements.
  --
  -- >>> matrixFromRows @(Matrix 2 3 Int) (Vector2 (Vector3 1 2 3) (Vector3 4 5 6))
  -- Matrix (Vector2 (Vector3 1 2 3) (Vector3 4 5 6))
  matrixFromRows :: ( Vector_ rowVector n (Vector m r)
                    , OptVector_ m r
                    ) => rowVector -> matrix

  -- | Matrix multiplication
  --
  -- >>> let m1  = matrixFromRows @(Matrix 2 3 Int) (Vector2 (Vector3 1 2 3) (Vector3 4 5 6))
  -- >>> let r i = Vector4 i (i*10) (i*100) (i*1000)
  -- >>> let m2  = matrixFromRows @(Matrix 3 4 Int) (Vector3 (r 1) (r 2) (r 3))
  -- >>> mapMOf_ rows print $ (m1 !*! m2 :: Matrix 2 4 Int)
  -- Vector4 14 140 1400 14000
  -- Vector4 32 320 3200 32000
  (!*!)     :: ( Matrix_ matrix'  m m' r
               , Matrix_ matrix'' n m' r
               , Num r
               , OptVector_ m r, KnownNat m
               , OptVector_ n r, KnownNat n
               , Additive_ (Vector m r)
               ) => matrix -> matrix' -> matrix''
  ma !*! mb = generateMatrix $ \(i,j) -> row' i ma `dot'` column' j mb
    where
      row' i    = fromMaybe (error "absurd: row i out of range") . row i
      column' j = fromMaybe (error "absurd: column j out of range") . column j

      dot' u v = sumOf components $ liftI2 (*) u v
  {-# INLINE (!*!) #-}

  -- | Multiply a matrix and a vector.
  --
  -- >>> let m = matrixFromRows @(Matrix 2 3 Int) (Vector2 (Vector3 1 2 3) (Vector3 4 5 6))
  -- >>> m !* Vector3 2 3 1
  -- Vector2 11 29
  (!*)  :: forall vector.
           ( Vector_ vector  m r
           , Additive_ vector
           , OptVector_ n r, OptVector_ m r
           , Num r
           ) => matrix -> vector -> Vector n r
  m !* v = vectorFromVector $ rows'&components' %~ dotWithV
    where
      components' :: Traversal (ListVector n a) (ListVector n b) a b
      components' = components
      rows'  = ListVector @n $ m^..rows
      dotWithV   :: Vector m r -> r
      dotWithV u = sumOf components $ liftI2 (*) (vectorFromVector @_ @vector u) v
  {-# INLINE (!*) #-}

  -- | traversal over all rows
  rows :: IndexedTraversal' Int matrix (Vector m r)

--  -- | Traversal over all columns
--  columns :: IndexedTraversal' Int matrix (Vector n r)



  -- | Access the i^th row in the matrix.
  --
  row     :: (OptVector_ m r, KnownNat m) => Int -> matrix -> Maybe (Vector m r)
  row i m = generateA $ \j -> m ^? ix (i,j)
  {-# INLINE row #-}

  -- row'   :: (OptVector_ m r, KnownNat m) => Int -> Traversal' matrix (Vector m r)
  -- row' i = traversal go
  --   where
  --     go focus m =
    -- generate $ \j -> m ^?! ix (i,j)

  -- | Access the j^th column in the matrix.
  column     :: (OptVector_ n r, KnownNat n) => Int -> matrix -> Maybe (Vector n r)
  column j m = generateA $ \i -> m ^? ix (i,j)
  {-# INLINE column #-}



infixl 7 !*!
infixl 7 !*


-- class Matrix_ matrix n m r => ConstructableMatrix_ matrix n m r where
--   fromList




-- | Dimensions for which we can compute the determinant of a matrix
class HasDeterminant d where
  det :: (Num r, Matrix_ matrix d d r) => matrix -> r

instance HasDeterminant 1 where
  det m = m^?!ix (0,0)




-- | Class of matrices that are invertible.
class Invertible n r where
  inverse' :: (Matrix_ matrix n n r) => matrix -> matrix





-- -- | Creates a row with zeroes everywhere, except at position i, where the
-- -- value is the supplied value.
-- mkRow     :: forall vector d r. (Vector_ vector d r, Num r) => Int -> r -> vector
-- mkRow i x = set (unsafeComponent i) x zero


--------------------------------------------------------------------------------
