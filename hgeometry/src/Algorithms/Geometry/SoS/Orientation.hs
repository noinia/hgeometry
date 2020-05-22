module Algorithms.Geometry.SoS.Orientation where

import           Algorithms.Geometry.SoS.Expr
import           Algorithms.Geometry.SoS.RWithIdx
import           Control.Lens
import           Control.Monad.ST.Strict
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.Point.Class
import           Data.Geometry.Point.Internal
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import qualified Data.Geometry.Vector as GV
import           Data.Geometry.Vector hiding (imap)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Ord (Down(..))
import           Data.Reflection
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           GHC.TypeNats
import           Linear.Matrix
import           Linear.V2 (V2(..))
import           Linear.V3 (V3(..))
import           Linear.V4 (V4(..))

--------------------------------------------------------------------------------

-- | To support Simulation of Simplicity a point type p must support:
--
-- - Retrieving the Index of the point
-- - The dimension of p must support SoS
type SoS p = ( AsPoint p
             , HasIndex p
             , SoSD (Dimension p)
             )

-- | A dimension d has support for SoS when we can:
--
-- - sort a vector that has (d+1) entries and count the number of
-- - exchanges made generate the terms of the determinant of the
--   \(\Lambda_{d+1}\) matrix in the right order.
type SoSD d = ( SortI (d + 1)
              , ToTerms d
              )



-- | Given a query point q, and a vector of d points defining a
-- hyperplane test if q lies above or below the hyperplane.
--
sideTest      :: ( SoS p
                 , d ~ Dimension p, Arity d, Arity (d+1)
                 , r ~ NumType p, Num r, Eq r
                 )
              => p -> Vector d p -> Sign
sideTest q ps = case bimap odd signDet $ sort q ps of
                  (True,  d) -> flipSign d
                  (False, d) -> d

{-# SPECIALIZE sideTest ::
  (CanAquire i (Point 1 r), Num r, Eq r) => P i 1 r -> Vector 1 (P i 1 r) -> Sign #-}
{-# SPECIALIZE sideTest ::
  (CanAquire i (Point 2 r), Num r, Eq r) => P i 2 r -> Vector 2 (P i 2 r) -> Sign #-}
{-# SPECIALIZE sideTest ::
  (CanAquire i (Point 3 r), Num r, Eq r) => P i 3 r -> Vector 3 (P i 3 r) -> Sign #-}


-- | Returns the number of comparisons and the sorted matrix, in which the rows are
-- sorted in increasing order of the indices.
sort      :: forall p d r
          . (d ~ Dimension p, Arity d, Arity (d+1), SortI (d + 1)
            , r ~ NumType p
            , AsPoint p, HasIndex p
            )
          => p -> Vector d p -> (Int, Vector (d + 1) (Vector d r))
sort q ps = second (fmap asVec) . sortI . fmap (with indexOf) $ q `GV.cons` ps
  where
    asVec (With _ p) = toVec . asPoint $ p

{-# SPECIALIZE sort ::
  CanAquire i (Point 1 r) => P i 1 r -> Vector 1 (P i 1 r) -> (Int, Vector 2 (Vector 1 r)) #-}
{-# SPECIALIZE sort ::
  CanAquire i (Point 2 r) => P i 2 r -> Vector 2 (P i 2 r) -> (Int, Vector 3 (Vector 2 r)) #-}
{-# SPECIALIZE sort ::
  CanAquire i (Point 3 r) => P i 3 r -> Vector 3 (P i 3 r) -> (Int, Vector 4 (Vector 3 r)) #-}


-- | Data type for p values that have an index.
data With p = With {-# UNPACK #-} !Int p deriving (Show)

with     :: (p -> Int) -> p -> With p
with f p = With (f p) p

instance Eq (With p) where
  (With i _) == (With j _) = i == j
instance Ord (With p) where
  (With i _) `compare` (With j _) = i `compare` j

--------------------------------------------------------------------------------

class SortI d where
  sortI :: Ord a => Vector d a -> (Int, Vector d a)

instance SortI 1 where
  sortI v = (0,v)
  {-# INLINE sortI #-}

instance SortI 2 where
  sortI v@(Vector2 i j) = case i `compare` j of
                            GT -> (1, Vector2 j i)
                            _  -> (0, v)
  {-# INLINE sortI #-}

-- | Based on the optimal sort in vector-algorithms
instance SortI 3 where
  sortI v@(Vector3 i j k) =
    case compare i j of
      GT -> case compare i k of
              GT -> case compare k j of
                      LT -> (1, Vector3 k j i)
                      _  -> (2, Vector3 j k i)
              _  -> (1, Vector3 j i k)
      _  -> case compare j k of
              GT -> case compare i k of
                      GT -> (2, Vector3 k i j)
                      _  -> (1, Vector3 i k j)
              _  -> (0,v)
  {-# INLINE sortI #-}

instance SortI 4 where
  sortI v@(Vector4 i j k l) =
    case compare i j of
        GT -> case compare i k of
                GT -> case compare j k of
                        GT -> case compare j l of
                                GT -> case compare k l of
                                        GT -> (2, Vector4 l k j i)
                                        _  -> (3, Vector4 k l j i)
                                _  -> case compare i l of
                                        GT -> (2, Vector4 k j l i)
                                        _  -> (1, Vector4 k j i l)
                        _ -> case compare k l of
                               GT -> case compare j l of
                                       GT -> (1, Vector4 l j k i)
                                       _  -> (2, Vector4 j l k i)
                               _  -> case compare i l of
                                       GT -> (3, Vector4 j k l i)
                                       _  -> (2, Vector4 j k i l)
                _  -> case compare i l of
                        GT -> case compare j l of
                                GT -> (2, Vector4 l j i k)
                                _  -> (3, Vector4 j l i k)
                        _  -> case compare k l of
                                GT -> (2, Vector4 j i l k)
                                _  -> (1, Vector4 j i k l)
        _  -> case compare j k of
                GT -> case compare i k of
                        GT -> case compare i l of
                                GT -> case compare k l of
                                        GT -> (3, Vector4 l k i j)
                                        _  -> (2, Vector4 k l i j)
                                _  -> case compare j l of
                                        GT -> (3, Vector4 k i l j)
                                        _  -> (2, Vector4 k i j l)
                        _  -> case compare k l of
                                GT -> case compare i l of
                                        GT -> (2, Vector4 l i k j)
                                        _  -> (1, Vector4 i l k j)
                                _  -> case compare j l of
                                        GT -> (2, Vector4 i k l j)
                                        _  -> (1, Vector4 i k j l)
                _  -> case compare j l of
                        GT -> case compare i l of
                                GT -> (3, Vector4 l i j k)
                                _  -> (2, Vector4 i l j k)
                        _  -> case compare k l of
                                GT -> (1, Vector4 i j l k)
                                _  -> (0, v)
  {-# INLINE sortI #-}


-- | Determines the sign of the Determinant.
--
-- pre: the rows in the input vector are given in increasing index
-- order
signDet :: (Num r, Eq r, ToTerms d) => Vector (d + 1) (Vector d r) -> Sign
signDet = signFromTerms . toTerms

{-# SPECIALIZE signDet :: (Num r, Eq r) => Vector 2 (Vector 1 r) -> Sign #-}
{-# SPECIALIZE signDet :: (Num r, Eq r) => Vector 3 (Vector 2 r) -> Sign #-}
{-# SPECIALIZE signDet :: (Num r, Eq r) => Vector 4 (Vector 3 r) -> Sign #-}

--------------------------------------------------------------------------------

class ToTerms d where
  toTerms :: Num r => Vector (d + 1) (Vector d r) -> [r]

instance ToTerms 1 where
  -- note this is Lambda_2 from the paper
  toTerms (Vector2 (Vector1 i)
                   (Vector1 j)) = [ i - j -- det22 [i 1, j 1] = i*1 - j*1
                                  , 1
                                  ]
  {-# INLINE toTerms #-}

instance ToTerms 2 where
  -- note this is Lambda_3 from the paper
  toTerms (Vector3 (Vector2 i1 i2)
                   (Vector2 j1 j2)
                   (Vector2 k1 k2)) = [ det33 $ V3 (V3 i1 i2 1)
                                                   (V3 j1 j2 1)
                                                   (V3 k1 k2 1)
                                      , -(j1 - k1) --

                                      , j2 - k2    -- det22 [[j2, 1], [k2, 1]] = j2*1 - k2*1
                                      , i1 - k1
                                      , 1
                                      ]
  {-# INLINE toTerms #-}


instance ToTerms 3 where
  -- note this is Lambda_4 from the paper
  toTerms (Vector4 (Vector3 i1 i2 i3)
                   (Vector3 j1 j2 j3)
                   (Vector3 k1 k2 k3)
                   (Vector3 l1 l2 l3)
          ) = [ det44 $ V4 (V4 i1 i2 i3 1)   -- 0
                           (V4 j1 j2 j3 1)
                           (V4 k1 k2 k3 1)
                           (V4 l1 l2 l3 1)
              , det33 $ V3 (V3 j1 j2 1)      -- 1
                           (V3 k1 k2 1)
                           (V3 l1 l2 1)
              , ((-1) *) .                   -- 2
                det33 $ V3 (V3 j1 j3 1)
                           (V3 k1 k3 1)
                           (V3 l1 l3 1)
              , det33 $ V3 (V3 j2 j3 1)      -- 3
                           (V3 k2 k3 1)
                           (V3 l2 l3 1)
              , ((-1) *) .                   -- 4
                det33 $ V3 (V3 i1 i2 1)
                           (V3 k1 k2 1)
                           (V3 l1 l2 1)
              , k1 - l1                      -- 5
              , -(k2 - l2)                   -- 6
              , det33 $ V3 (V3 i1 i3 1)      -- 7
                           (V3 k1 k3 1)
                           (V3 l1 l3 1)
              , k3 - l3                      -- 8
              , ((-1) *) .                   -- 9
                det33 $ V3 (V3 i2 i3 1)
                           (V3 k2 k3 1)
                           (V3 l2 l3 1)
              , det33 $ V3 (V3 i1 i2 1)      -- 10
                           (V3 j1 j2 1)
                           (V3 l1 l2 1)
              , -(j1 - l1)                   -- 11
              , j2 - l2                      -- 12
              , i1 - l1                      -- 13
              , 1                            -- 14
              ]
  {-# INLINE toTerms #-}
