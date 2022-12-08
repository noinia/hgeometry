{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Unpacked.V2
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of 2 dimensional vectors for unpackable types.
--
--------------------------------------------------------------------------------
module V2
  ( Vec2(..)
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


--------------------------------------------------------------------------------

-- type Point2 = PointF Vec2

--------------------------------------------------------------------------------

-- | Unboxed 2D vectors
data Vec2 = Vec2 {-# UNPACK #-} !R
                 {-# UNPACK #-} !R
            deriving (Show,Eq,Ord,Generic)

type instance Dimension Vec2 = 2

type instance NumType   Vec2 = R
type instance IxValue   Vec2 = R
type instance Index     Vec2 = Int

instance NFData Vec2

instance Random Vec2 where
  randomR (lows,highs) g0 = flip runState g0 $
                            zipWithM' (\l h -> state $ randomR (l,h)) lows highs
  random g0 = flip runState g0 $ replicateM' (state random)

instance UniformRange Vec2 where
  uniformRM (lows,highs) gen = zipWithM' (\l h -> uniformRM (l,h) gen) lows highs

zipWithM' :: Applicative m => (R -> R -> m R) -> Vec2 -> Vec2 -> m Vec2
zipWithM' f (Vec2 x y) (Vec2 x' y') = Vec2 <$> f x x' <*> f y y'

replicateM'   :: Applicative f => f R -> f Vec2
replicateM' x = Vec2 <$> x <*> x

-- instance Uniform Vec2 where
--   uniformM gen = replicateM' (uniformM gen)

instance FromJSON Vec2
instance ToJSON Vec2 where
  toEncoding = genericToEncoding defaultOptions


instance Field1 Vec2 Vec2 R R where
  _1 = lens (\(Vec2 x _) -> x) (\(Vec2 _ y) x -> Vec2 x y)
instance Field2 Vec2 Vec2 R R where
  _2 = lens (\(Vec2 _ y) -> y) (\(Vec2 x _) y -> Vec2 x y)


instance Ixed Vec2 where
  ix i f v@(Vec2 x y) = case i of
                          0 -> flip Vec2 y <$> f x
                          1 -> Vec2 x      <$> f y
                          _ -> pure v
  {-# INLINE ix #-}

instance Vector_ v 2 (IxValue v) => HasComponents Vec2 v where
  {-# SPECIALIZE instance HasComponents Vec2 Vec2 #-}
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'               :: Applicative f => (R -> f (IxValue v)) -> Vec2 -> f v
      traverse' f (Vec2 x y)  = Vector2_ <$> f x <*> f y
      itraverse'              :: Applicative f => (Int -> R -> f (IxValue v)) -> Vec2 -> f v
      itraverse' f (Vec2 x y) = Vector2_ <$> f 0 x <*> f 1 y
  {-# INLINE components #-}


instance Vector_ Vec2 2 R where
--  mkVector = Vec2
  vectorFromList = \case
    [x,y] -> Just $ Vec2 x y
    _     -> Nothing
  {-# INLINE vectorFromList #-}

instance ConstructableVector_ Vec2 2 R where
  mkVector = Vec2

instance Additive_ Vec2 where
  zero   = Vec2 0 0
  liftU2 f (Vec2 x y) (Vec2 x' y') = Vec2 (f x x') (f y y')
  liftI2 f (Vec2 x y) (Vec2 x' y') = Vec2 (f x x') (f y y')

instance Metric_ Vec2


-- | elements of the vector are stored consecutively
newtype instance UMV.MVector s Vec2 = MV_Vec2 (UMV.MVector s R)
newtype instance UV.Vector     Vec2 = V_Vec2  (UV.Vector     R)

instance GMV.MVector UMV.MVector Vec2 where
  basicLength (MV_Vec2 v) = GMV.basicLength v `div` 2
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_Vec2 v) = MV_Vec2 $ GMV.basicUnsafeSlice (2*s) (2*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_Vec2 v) (MV_Vec2 v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Vec2 <$> GMV.basicUnsafeNew (2*n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Vec2 v) = GMV.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_Vec2 v) i = do x <- GMV.basicUnsafeRead v  (2*i)
                                     y <- GMV.basicUnsafeRead v (2*i+1)
                                     pure $ Vec2 x y
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Vec2 v) i (Vec2 x y) = do GMV.basicUnsafeWrite v (2*i)   x
                                                 GMV.basicUnsafeWrite v (2*i+1) y
  {-# INLINE basicUnsafeWrite #-}


-- type instance GV.Mutable UV.Vector2

instance GV.Vector UV.Vector Vec2 where

  basicUnsafeFreeze (MV_Vec2 mv) = V_Vec2 <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Vec2 v) = MV_Vec2 <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Vec2 v) = GV.basicLength v `div` 2
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_Vec2 v) = V_Vec2 $ GV.basicUnsafeSlice (2*s) (2*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Vec2 v) i = Vec2 <$> GV.basicUnsafeIndexM v (2*i)
                                        <*> GV.basicUnsafeIndexM v (2*i+1)
  {-# INLINE basicUnsafeIndexM #-}

instance UV.Unbox Vec2
