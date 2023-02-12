{-# OPTIONS_GHC -ddump-simpl -dsuppress-module-prefixes -dsuppress-uniques -ddump-to-file #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  V4
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of 4 dimensional vectors for unpackable types.
--
--------------------------------------------------------------------------------
module V4
  ( Vector(..)
  ) where

import           Control.DeepSeq
import           Control.Lens
-- import           Control.Monad.State
import qualified Data.Functor.Apply as Apply
import qualified Data.Vector.Generic as GV
import           Data.Vector.Generic.Mutable (MVector(basicInitialize))
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           GHC.Generics (Generic)
import           HGeometry.Properties
import           HGeometry.Vector.Class
import           R
-- import           System.Random (Random (..))
-- import           System.Random.Stateful (UniformRange(..) ) -- , Uniform(..))

--------------------------------------------------------------------------------

-- | Unboxed dD vectors
data instance Vector 4 R = Vector4 {-# UNPACK #-} !R
                                   {-# UNPACK #-} !R
                                   {-# UNPACK #-} !R
                                   {-# UNPACK #-} !R
                         deriving (Eq,Ord,Generic)

instance NFData (Vector 4 R)

-- instance Random (Vector 4 R) where

-- instance UniformRange (Vector 4 R) where
--   uniformRM (lows
--             ,highs) gen = zipWithM' (\l h -> uniformRM (l,h) gen) lows highs

-- zipWithM'                                    :: Applicative m
--                                              => (R -> R -> m R) -> Vec4 -> Vec4 -> m Vec4
-- zipWithM' f (Vec4 x y z w) (Vec4 x' y' z' w') = Vec4 <$> f x x' <*> f y y' <*> f z z' <*> f w w'

-- replicateM'   :: Applicative f => f R -> f Vec4
-- replicateM' x = Vec4 <$> x <*> x <*> x <*> x

-- instance Uniform Vec4 where
--   uniformM gen = replicateM' (uniformM gen)

instance Field1 (Vector 4 R) (Vector 4 R) R R where
  _1 = lens (\(Vector4 x _ _ _) -> x) (\(Vector4 _ y z w) x -> Vector4 x y z w)
  {-# INLINE _1 #-}
instance Field2 (Vector 4 R) (Vector 4 R) R R where
  _2 = lens (\(Vector4 _ y _ _) -> y) (\(Vector4 x _ z w) y -> Vector4 x y z w)
  {-# INLINE _2 #-}
instance Field3 (Vector 4 R) (Vector 4 R) R R where
  _3 = lens (\(Vector4 _ _ z _) -> z) (\(Vector4 x y _ w) z -> Vector4 x y z w)
  {-# INLINE _3 #-}
instance Field4 (Vector 4 R) (Vector 4 R) R R where
  _4 = lens (\(Vector4 _ _ _ w) -> w) (\(Vector4 x y z _) w -> Vector4 x y z w)
  {-# INLINE _4 #-}

instance Ixed (Vector 4 R) where
  ix i f v@(Vector4 x y z w) = case i of
                                 0 -> (\x' -> Vector4 x' y z w) <$> f x
                                 1 -> (\y' -> Vector4 x y' z w) <$> f y
                                 2 -> (\z' -> Vector4 x y z' w) <$> f z
                                 3 -> (\w' -> Vector4 x y z w') <$> f w
                                 _ -> pure v
  {-# INLINE ix #-}

instance VectorLike_ (Vector 4 R) where
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'  :: Apply.Apply f => (R -> f R) -> Vector 4 R -> f (Vector 4 R)
      traverse' f (Vector4 x y z w)  = Vector4 <$> f x Apply.<.> f y Apply.<.> f z Apply.<.> f w
      itraverse' :: Apply.Apply f => (Int -> R -> f R) -> Vector 4 R -> f (Vector 4 R)
      itraverse' f (Vector4 x y z w) = Vector4 <$> f 0 x Apply.<.> f 1 y Apply.<.> f 2 z Apply.<.> f 3 w
  {-# INLINE components #-}

instance Additive_ (Vector 4 R) where
  zero   = Vector4 0 0 0 0
  {-# INLINE zero #-}
  liftU2 f (Vector4 x y z w) (Vector4 x' y' z' w') = Vector4 (f x x') (f y y') (f z z') (f w w')
  {-# INLINE liftU2 #-}
  liftI2 f (Vector4 x y z w) (Vector4 x' y' z' w') = Vector4 (f x x') (f y y') (f z z') (f w w')
  {-# INLINE liftI2 #-}


{-

-- | elements of the vector are stored consecutively
newtype instance UMV.MVector s (Vector 4 R) = MV_Vector4 (UMV.MVector s R)
newtype instance UV.Vector     (Vector 4 R) = V_Vector4  (UV.Vector     R)

instance GMV.MVector UMV.MVector (Vector 4 R) where
  basicLength (MV_Vector4 v) = GMV.basicLength v `div` 4
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_Vector4 v) = MV_Vector4 $ GMV.basicUnsafeSlice (4*s) (4*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_Vector4 v) (MV_Vector4 v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Vector4 <$> GMV.basicUnsafeNew (4*n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Vector4 v) = GMV.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_Vector4 v) i = do x <- GMV.basicUnsafeRead v (4*i)
                                        y <- GMV.basicUnsafeRead v (4*i+1)
                                        z <- GMV.basicUnsafeRead v (4*i+2)
                                        w <- GMV.basicUnsafeRead v (4*i+3)
                                        pure $ Vector4 x y z w
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Vector4 v) i (Vector4 x y z w) = do GMV.basicUnsafeWrite v (4*i)   x
                                                           GMV.basicUnsafeWrite v (4*i+1) y
                                                           GMV.basicUnsafeWrite v (4*i+2) z
                                                           GMV.basicUnsafeWrite v (4*i+3) w
  {-# INLINE basicUnsafeWrite #-}


instance GV.Vector UV.Vector (Vector 4 R) where

  basicUnsafeFreeze (MV_Vector4 mv) = V_Vector4 <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Vector4 v) = MV_Vector4 <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Vector4 v) = GV.basicLength v `div` 4
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_Vector4 v) = V_Vector4 $ GV.basicUnsafeSlice (4*s) (4*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Vector4 v) i = Vector4 <$> GV.basicUnsafeIndexM v (4*i)
                                              <*> GV.basicUnsafeIndexM v (4*i+1)
                                              <*> GV.basicUnsafeIndexM v (4*i+2)
                                              <*> GV.basicUnsafeIndexM v (4*i+3)
  {-# INLINE basicUnsafeIndexM #-}

instance UV.Unbox (Vector 4 R)

-}
