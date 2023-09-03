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
-- import           HGeometry.Vector.List (ListVector(..))
import           Prelude hiding (zipWith)

--------------------------------------------------------------------------------
-- $setup
-- >>> import HGeometry.Point
-- >>> import HGeometry.Matrix
-- >>> let printMatrix = mapMOf_ (rows.traverse) print
-- >>> :{
-- let matrixFromList' :: ( Matrix_ matrix n m r
--                        , KnownNat n, KnownNat m
--                        , Has_ Vector_  m r
--                        , Has_ Vector_  n (Vector m r)
--                        ) => [r] -> matrix
--     matrixFromList' = fromMaybe (error "") . matrixFromList
-- :}


-- | Types that have an 'elements' field lens.
class HasElements matrix matrix' where
  -- | IndexedTraversal over the elements of the matrix, each index is
  -- a (row,column) index pair.
  elements :: IndexedTraversal1 (Int,Int) matrix matrix' (NumType matrix) (NumType matrix')

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
  -- >>> printMatrix $ identityMatrix @(Matrix 3 3 Int)
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
                       , Has_ Vector_  m r
                       , Has_ Vector_  n (Vector m r)
                       ) => [r] -> Maybe matrix
  matrixFromList xs = do rs  <- go n xs
                         rs' <- vectorFromList @(Vector n (Vector m r)) rs
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
                    -- , OptVector_ m r
                    ) => rowVector -> matrix

  -- | Matrix multiplication
  --
  -- >>> let m1  = matrixFromRows @(Matrix 2 3 Int) (Vector2 (Vector3 1 2 3) (Vector3 4 5 6))
  -- >>> let r i = Vector4 i (i*10) (i*100) (i*1000)
  -- >>> let m2  = matrixFromRows @(Matrix 3 4 Int) (Vector3 (r 1) (r 2) (r 3))
  -- >>> printMatrix $ (m1 !*! m2 :: Matrix 2 4 Int)
  -- Vector4 14 140 1400 14000
  -- Vector4 32 320 3200 32000
  (!*!)     :: ( Matrix_ matrix'  m m' r
               , Matrix_ matrix'' n m' r
               , Num r
               -- , OptVector_ m r, KnownNat m
               -- , OptVector_ n r, KnownNat n
               , Has_ Additive_ m r -- (Vector m r)
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
  (!*)  :: ( Has_ Vector_ n (Vector m r)
           , HasComponents (Vector n (Vector m r)) (Vector n r)
           -- , Additive_
           -- , OptVector_ n r, OptVector_ m r
           , Has_ Additive_ m r
           , Num r
           ) => matrix -> Vector m r -> Vector n r
  m !* v = rows'&components %~ dotWithV
    where
      rows'  :: Vector n (Vector m r)
      rows'  = m^.rows
      dotWithV   :: Vector m r -> r
      dotWithV u = sumOf components $ liftI2 (*) u (v^._Vector)
  {-# INLINE (!*) #-}

  -- | Multiply a scalar and a matrix
  (*!!)   :: Num r => r -> matrix -> matrix
  s *!! m = m&elements *~ s
  {-# INLINE (*!!) #-}

  -- | Multiply a matrix and a scalar
  (!!*)   :: Num r => matrix -> r -> matrix
  m !!* s = m&elements *~ s
  {-# INLINE (!!*) #-}

  -- | Lens to access all rows
  rows :: Lens' matrix (Vector n (Vector m r))
  -- rows :: IndexedTraversal' Int matrix (Vector m r)

--  -- | Traversal over all columns
--  columns :: IndexedTraversal' Int matrix (Vector n r)



  -- | Access the i^th row in the matrix.
  --
  row     :: Has_ Vector_ m r => Int -> matrix -> Maybe (Vector m r)
  row i m = generateA $ \j -> m ^? ix (i,j)
  {-# INLINE row #-}

  -- row'   :: (OptVector_ m r, KnownNat m) => Int -> Traversal' matrix (Vector m r)
  -- row' i = traversal go
  --   where
  --     go focus m =
    -- generate $ \j -> m ^?! ix (i,j)

  -- | Access the j^th column in the matrix.
  column     :: Has_ Vector_ n r => Int -> matrix -> Maybe (Vector n r)
  column j m = generateA $ \i -> m ^? ix (i,j)
  {-# INLINE column #-}


infixl 7 !*!
infixl 7 !*
infixl 7 *!!
infixl 7 !!*

-- class Matrix_ matrix n m r => ConstructableMatrix_ matrix n m r where
--   fromList


--------------------------------------------------------------------------------
-- * Determinants

-- | Dimensions for which we can compute the determinant of a matrix
class HasDeterminant d where
  det :: (Num r, Matrix_ matrix d d r) => matrix -> r

instance HasDeterminant 1 where
  det m = m^?!ix (0,0)
  {-# INLINE det #-}

instance HasDeterminant 2 where
  det m = case m^..elements of
            [ a, b,
              c, d] -> det22 a b c d -- a*d - b*c
            _       -> error "det: 2x2, absurd"
  {-# INLINE det #-}

-- | determinant of a 2x2 matrix [[a,b], [c,d]]
det22         :: Num r => r -> r -> r -> r -> r
det22 a b c d = a*d - b*c

instance HasDeterminant 3 where
  det m = case m^..elements of
            [ a, b, c,
              d, e, f,
              g, h, i] -> a*e*i + b*f*g + c*d*h - c*e*g - b*d*i - a*f*h
            _          -> error "det: 3x3, absurd"
  {-# INLINE det #-}

instance HasDeterminant 4 where
  det m = case m^..elements of
    [ i00, i01, i02, i03,
      i10, i11, i12, i13,
      i20, i21, i22, i23,
      i30, i31, i32, i33 ] -> let s0 = i00 * i11 - i10 * i01
                                  s1 = i00 * i12 - i10 * i02
                                  s2 = i00 * i13 - i10 * i03
                                  s3 = i01 * i12 - i11 * i02
                                  s4 = i01 * i13 - i11 * i03
                                  s5 = i02 * i13 - i12 * i03

                                  c5 = i22 * i33 - i32 * i23
                                  c4 = i21 * i33 - i31 * i23
                                  c3 = i21 * i32 - i31 * i22
                                  c2 = i20 * i33 - i30 * i23
                                  c1 = i20 * i32 - i30 * i22
                                  c0 = i20 * i31 - i30 * i21
                              in s0 * c5 - s1 * c4 + s2 * c3 + s3 * c2 - s4 * c1 + s5 * c0
     -- adapted from the implementation in the Linear package.
    _ -> error "det: 4x4 absurd"
  {-# INLINE det #-}
  -- TODO: verify that GHC unrolls the list
  -- TODO verify that GHC specializes this for the most relevant types

--------------------------------------------------------------------------------
-- * Invertible matrices


-- | Class of matrices that are invertible.
class Invertible n where
  -- | given an invertable square \(n \times n\) matrix A, computes
  -- the \(n \times n\) matrix B such that A !*! B = identityMatrix
  --
  inverseMatrix :: ( Fractional r
                   , Matrix_ matrix n n r
                   , Has_ Vector_ n r
                   ) => matrix -> matrix

instance Invertible 1 where
  inverseMatrix m = m&elements %~ (\x -> (1/x))
  -- slightly weird way of writing this, since there is only one element, but whatever
  {-# INLINE inverseMatrix #-}

instance Invertible 2 where
  -- >>> printMatrix $ inverseMatrix $ matrixFromList' @(Matrix 2 2 Double) [1,2, 3,4]
  -- (Vector2 (-2.0) 1.0)
  -- (Vector2 1.5 (-0.5))
  inverseMatrix m = case m^..elements of
                      [a,b,
                       c,d] -> let s = 1 / det m
                               in s *!! (matrixFromRows $ Vector2 -- @(ListVector 2 _)
                                  (Vector2 d          (negate b))
                                  (Vector2 (negate c) a))
                      _     -> error "inverseMatrix 2x2: absurd"
  {-# INLINE inverseMatrix #-}
  -- it is a bit silly we are using the list vectors here.

instance Invertible 3 where
  -- >>> printMatrix $ inverseMatrix $ matrixFromList' @(Matrix 3 3 Double) [1,2,4,     4,2,2,    1,1,1]
  -- (Vector3 0.0 0.5 (-1.0))
  -- (Vector3 (-0.5) (-0.75) 3.5)
  -- (Vector3 0.5 0.25 (-1.5))
  inverseMatrix m = case m^..elements of
      [ a, b, c,
        d, e, f,
        g, h, i] -> let lambda  = 1 / det m
                        aa = det22 e f h i
                        bb = det22 c b i h
                        cc = det22 b c e f
                        dd = det22 f d i g
                        ee = det22 a c g i
                        ff = det22 c a f d
                        gg = det22 d e g h
                        hh = det22 b a h g
                        ii = det22 a b d e
                    in lambda *!! (matrixFromRows $ Vector3 -- _ @(ListVector 3 _)
                                     (Vector3 aa bb cc)
                                     (Vector3 dd ee ff)
                                     (Vector3 gg hh ii))
      _          -> error "inverseMatrix 3x3: absurd"
  {-# INLINE inverseMatrix #-}

--------------------------------------------------------------------------------
