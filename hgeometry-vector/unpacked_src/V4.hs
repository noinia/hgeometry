{-# LANGUAGE UndecidableInstances #-}
module V4
  ( Vec4(..)
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
data Vec4 = Vec4 {-# UNPACK #-} !R
                 {-# UNPACK #-} !R
                 {-# UNPACK #-} !R
                 {-# UNPACK #-} !R
            deriving (Show,Eq,Ord,Generic)

type instance Dimension Vec4 = 4

type instance NumType   Vec4 = R
type instance IxValue   Vec4 = R
type instance Index     Vec4 = Int

instance NFData Vec4

instance Random Vec4 where
  randomR (lows,highs) g0 = flip runState g0 $
                            zipWithM' (\l h -> state $ randomR (l,h)) lows highs
  random g0 = flip runState g0 $ replicateM' (state random)

instance UniformRange Vec4 where
  uniformRM (lows,highs) gen = zipWithM' (\l h -> uniformRM (l,h) gen) lows highs

zipWithM'                                    :: Applicative m
                                             => (R -> R -> m R) -> Vec4 -> Vec4 -> m Vec4
zipWithM' f (Vec4 x y z w) (Vec4 x' y' z' w') = Vec4 <$> f x x' <*> f y y' <*> f z z' <*> f w w'

replicateM'   :: Applicative f => f R -> f Vec4
replicateM' x = Vec4 <$> x <*> x <*> x <*> x

-- instance Uniform Vec4 where
--   uniformM gen = replicateM' (uniformM gen)

instance FromJSON Vec4
instance ToJSON Vec4 where
  toEncoding = genericToEncoding defaultOptions


instance Field1 Vec4 Vec4 R R where
  _1 = lens (\(Vec4 x _ _ _) -> x) (\(Vec4 _ y z w) x -> Vec4 x y z w)
instance Field2 Vec4 Vec4 R R where
  _2 = lens (\(Vec4 _ y _ _) -> y) (\(Vec4 x _ z w) y -> Vec4 x y z w)
instance Field3 Vec4 Vec4 R R where
  _3 = lens (\(Vec4 _ _ z _) -> z) (\(Vec4 x y _ w) z -> Vec4 x y z w)
instance Field4 Vec4 Vec4 R R where
  _4 = lens (\(Vec4 _ _ _ w) -> w) (\(Vec4 x y z _) w -> Vec4 x y z w)


instance Ixed Vec4 where
  ix i f v@(Vec4 x y z w) = case i of
                            0 -> (\x' -> Vec4 x' y z w) <$> f x
                            1 -> (\y' -> Vec4 x y' z w) <$> f y
                            2 -> (\z' -> Vec4 x y z' w) <$> f z
                            3 -> (\w' -> Vec4 x y z w') <$> f w
                            _ -> pure v
  {-# INLINE ix #-}

instance Vector_ v 4 (NumType v) => HasComponents Vec4 v where
  {-# SPECIALIZE instance HasComponents Vec4 Vec4 #-}
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'                   :: Applicative f => (R -> f (NumType v)) -> Vec4 -> f v
      traverse' f (Vec4 x y z w)  = Vector4_ <$> f x <*> f y <*> f z <*> f w
      itraverse'                 :: Applicative f => (Int -> R -> f (NumType v)) -> Vec4 -> f v
      itraverse' f (Vec4 x y z w) = Vector4_ <$> f 0 x <*> f 1 y <*> f 2 z <*> f 3 w
  {-# INLINE components #-}


instance Vector_ Vec4 4 R where
--  mkVector = Vec4
  vectorFromList = \case
    [x,y,z,w] -> Just $ Vec4 x y z w
    _         -> Nothing
  {-# INLINE vectorFromList #-}

instance Additive_ Vec4 where
  zero   = Vec4 0 0 0 0
  liftU2 f (Vec4 x y z w) (Vec4 x' y' z' w') = Vec4 (f x x') (f y y') (f z z') (f w w')
  liftI2 f (Vec4 x y z w) (Vec4 x' y' z' w') = Vec4 (f x x') (f y y') (f z z') (f w w')

instance Metric_ Vec4


-- | elements of the vector are stored consecutively
newtype instance UMV.MVector s Vec4 = MV_Vec4 (UMV.MVector s R)
newtype instance UV.Vector     Vec4 = V_Vec4  (UV.Vector     R)

instance GMV.MVector UMV.MVector Vec4 where
  basicLength (MV_Vec4 v) = GMV.basicLength v `div` 4
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_Vec4 v) = MV_Vec4 $ GMV.basicUnsafeSlice (4*s) (4*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_Vec4 v) (MV_Vec4 v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Vec4 <$> GMV.basicUnsafeNew (4*n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Vec4 v) = GMV.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_Vec4 v) i = do x <- GMV.basicUnsafeRead v (4*i)
                                     y <- GMV.basicUnsafeRead v (4*i+1)
                                     z <- GMV.basicUnsafeRead v (4*i+2)
                                     w <- GMV.basicUnsafeRead v (4*i+3)
                                     pure $ Vec4 x y z w
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Vec4 v) i (Vec4 x y z w) = do GMV.basicUnsafeWrite v (4*i)   x
                                                     GMV.basicUnsafeWrite v (4*i+1) y
                                                     GMV.basicUnsafeWrite v (4*i+2) z
                                                     GMV.basicUnsafeWrite v (4*i+3) w
  {-# INLINE basicUnsafeWrite #-}


-- type instance GV.Mutable UV.Vector2

instance GV.Vector UV.Vector Vec4 where

  basicUnsafeFreeze (MV_Vec4 mv) = V_Vec4 <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Vec4 v) = MV_Vec4 <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Vec4 v) = GV.basicLength v `div` 4
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_Vec4 v) = V_Vec4 $ GV.basicUnsafeSlice (4*s) (4*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Vec4 v) i = Vec4 <$> GV.basicUnsafeIndexM v (4*i)
                                        <*> GV.basicUnsafeIndexM v (4*i+1)
                                        <*> GV.basicUnsafeIndexM v (4*i+2)
                                        <*> GV.basicUnsafeIndexM v (4*i+3)
  {-# INLINE basicUnsafeIndexM #-}

instance UV.Unbox Vec4
