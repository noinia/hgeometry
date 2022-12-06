{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Unpacked.V3
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of 3 dimensional vectors for unpackable types.
--
--------------------------------------------------------------------------------
module V3
  ( Vec3(..)
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.State
import           Data.Aeson
import qualified Data.Vector.Generic as GV
import           Data.Vector.Generic.Mutable (MVector(basicInitialize))
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           GHC.Generics (Generic)
import           HGeometry.Properties
import           HGeometry.Vector.Class
import           R
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..) ) -- , Uniform(..))

--------------------------------------------------------------------------------

-- | Unboxed dD vectors
data Vec3 = Vec3 {-# UNPACK #-} !R
                 {-# UNPACK #-} !R
                 {-# UNPACK #-} !R
            deriving (Show,Eq,Ord,Generic)

type instance Dimension Vec3 = 3

type instance NumType   Vec3 = R
type instance IxValue   Vec3 = R
type instance Index     Vec3 = Int

instance NFData Vec3

instance Random Vec3 where
  randomR (lows,highs) g0 = flip runState g0 $
                            zipWithM' (\l h -> state $ randomR (l,h)) lows highs
  random g0 = flip runState g0 $ replicateM' (state random)

instance UniformRange Vec3 where
  uniformRM (lows,highs) gen = zipWithM' (\l h -> uniformRM (l,h) gen) lows highs

zipWithM'                                :: Applicative m
                                         => (R -> R -> m R) -> Vec3 -> Vec3 -> m Vec3
zipWithM' f (Vec3 x y z) (Vec3 x' y' z') = Vec3 <$> f x x' <*> f y y' <*> f z z'

replicateM'   :: Applicative f => f R -> f Vec3
replicateM' x = Vec3 <$> x <*> x <*> x

-- instance Uniform Vec3 where
--   uniformM gen = replicateM' (uniformM gen)

instance FromJSON Vec3
instance ToJSON Vec3 where
  toEncoding = genericToEncoding defaultOptions


instance Field1 Vec3 Vec3 R R where
  _1 = lens (\(Vec3 x _ _) -> x) (\(Vec3 _ y z) x -> Vec3 x y z)
instance Field2 Vec3 Vec3 R R where
  _2 = lens (\(Vec3 _ y _) -> y) (\(Vec3 x _ z) y -> Vec3 x y z)
instance Field3 Vec3 Vec3 R R where
  _3 = lens (\(Vec3 _ _ z) -> z) (\(Vec3 x y _) z -> Vec3 x y z)


instance Ixed Vec3 where
  ix i f v@(Vec3 x y z) = case i of
                            0 -> (\x' -> Vec3 x' y z) <$> f x
                            1 -> (\y' -> Vec3 x y' z) <$> f y
                            2 -> (\z' -> Vec3 x y z') <$> f z
                            _ -> pure v
  {-# INLINE ix #-}

instance Vector_ v 3 (NumType v) => HasComponents Vec3 v where
  {-# SPECIALIZE instance HasComponents Vec3 Vec3 #-}
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'                 :: Applicative f => (R -> f (NumType v)) -> Vec3 -> f v
      traverse' f (Vec3 x y z)  = Vector3_ <$> f x <*> f y <*> f z
      itraverse'               :: Applicative f => (Int -> R -> f (NumType v)) -> Vec3 -> f v
      itraverse' f (Vec3 x y z) = Vector3_ <$> f 0 x <*> f 1 y <*> f 2 z
  {-# INLINE components #-}


instance Vector_ Vec3 3 R where
--  mkVector = Vec3
  vectorFromList = \case
    [x,y,z] -> Just $ Vec3 x y z
    _       -> Nothing
  {-# INLINE vectorFromList #-}

instance ConstructableVector_ Vec3 3 R where
  mkVector = Vec3

instance Additive_ Vec3 where
  zero   = Vec3 0 0 0
  liftU2 f (Vec3 x y z) (Vec3 x' y' z') = Vec3 (f x x') (f y y') (f z z')
  liftI2 f (Vec3 x y z) (Vec3 x' y' z') = Vec3 (f x x') (f y y') (f z z')

instance Metric_ Vec3


-- | elements of the vector are stored consecutively
newtype instance UMV.MVector s Vec3 = MV_Vec3 (UMV.MVector s R)
newtype instance UV.Vector     Vec3 = V_Vec3  (UV.Vector     R)

instance GMV.MVector UMV.MVector Vec3 where
  basicLength (MV_Vec3 v) = GMV.basicLength v `div` 3
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_Vec3 v) = MV_Vec3 $ GMV.basicUnsafeSlice (3*s) (3*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_Vec3 v) (MV_Vec3 v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Vec3 <$> GMV.basicUnsafeNew (3*n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Vec3 v) = GMV.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_Vec3 v) i = do x <- GMV.basicUnsafeRead v (3*i)
                                     y <- GMV.basicUnsafeRead v (3*i+1)
                                     z <- GMV.basicUnsafeRead v (3*i+2)
                                     pure $ Vec3 x y z
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Vec3 v) i (Vec3 x y z) = do GMV.basicUnsafeWrite v (3*i)   x
                                                   GMV.basicUnsafeWrite v (3*i+1) y
                                                   GMV.basicUnsafeWrite v (3*i+2) z
  {-# INLINE basicUnsafeWrite #-}


-- type instance GV.Mutable UV.Vector2

instance GV.Vector UV.Vector Vec3 where

  basicUnsafeFreeze (MV_Vec3 mv) = V_Vec3 <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Vec3 v) = MV_Vec3 <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Vec3 v) = GV.basicLength v `div` 3
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_Vec3 v) = V_Vec3 $ GV.basicUnsafeSlice (3*s) (3*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Vec3 v) i = Vec3 <$> GV.basicUnsafeIndexM v (3*i)
                                        <*> GV.basicUnsafeIndexM v (3*i+1)
                                        <*> GV.basicUnsafeIndexM v (3*i+2)
  {-# INLINE basicUnsafeIndexM #-}

instance UV.Unbox Vec3
