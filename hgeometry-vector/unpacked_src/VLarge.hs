{-# LANGUAGE UndecidableInstances #-}
module VLarge
  ( VecLarge(..)
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.State
import           Data.Aeson
import           Data.Proxy
import qualified Data.Vector.Generic as GV
import           Data.Vector.Generic.Mutable (MVector(basicInitialize))
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           GHC.Generics (Generic)
import           GHC.TypeNats
import           HGeometry.Properties
import           HGeometry.Vector.Class
import           R
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..) ) -- , Uniform(..))

--------------------------------------------------------------------------------

-- | Unboxed large dD vectors

newtype VecLarge d = VecLarge (UV.Vector R)
  deriving (Generic)

deriving stock   instance Show (UV.Vector R)  => Show (VecLarge d)
deriving newtype instance Eq (UV.Vector R)  => Eq (VecLarge d)
deriving newtype instance Ord (UV.Vector R) => Ord (VecLarge d)

type instance Dimension (VecLarge d) = d

type instance NumType   (VecLarge d) = R
type instance IxValue   (VecLarge d) = R
type instance Index     (VecLarge d) = Int

instance NFData (VecLarge d)

instance KnownNat d => Random (VecLarge d) where
  randomR (lows,highs) g0 = flip runState g0 $
                            zipWithM' (\l h -> state $ randomR (l,h)) lows highs
  random g0 = flip runState g0 $ replicateM' (state random)

instance UniformRange (VecLarge d) where
  uniformRM (lows,highs) gen = zipWithM' (\l h -> uniformRM (l,h) gen) lows highs

zipWithM'                             :: Monad m
                                      => (R -> R -> m R) -> VecLarge d -> VecLarge d
                                      -> m (VecLarge d)
zipWithM' f (VecLarge v) (VecLarge w) = VecLarge <$> UV.zipWithM f v w

replicateM'   :: forall f d. (Monad f, KnownNat d) => f R -> f (VecLarge d)
replicateM' x = VecLarge <$> UV.replicateM d x
  where
    d = fromIntegral . natVal $ Proxy @d

_VecLargeVector :: Iso' (VecLarge d) (UV.Vector R)
_VecLargeVector = iso (\(VecLarge v) -> v) VecLarge

instance FromJSON (VecLarge d)
instance ToJSON (VecLarge d) where
  toEncoding = genericToEncoding defaultOptions

instance Ixed (VecLarge d) where
  ix i = _VecLargeVector.ix i


-- instance Vector_ v d (NumType v) => HasComponents (VecLarge d) v where
--   {-# SPECIALIZE instance HasComponents (VecLarge d) (VecLarge d) #-}
--   components = conjoined traverse' (itraverse' . indexed)
--     where
--       traverse'                   :: Applicative f => (R -> f (NumType v)) -> VecLarge d -> f v
--       traverse' f (VecLarge v)  = VecLarge <$> traverse'' f v
--       itraverse'                 :: Applicative f
--                                  => (Int -> R -> f (NumType v)) -> VecLarge d -> f v
--       itraverse' f (VecLarge v) = VecLarge <$> itraverse'' f v
--   {-# INLINE components #-}

instance HasComponents (VecLarge d) (VecLarge d) where
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'                   :: Applicative f => (R -> f R) -> VecLarge d -> f (VecLarge d)
      traverse' f (VecLarge v)  = VecLarge <$> traverse'' f v
      itraverse'                 :: Applicative f
                                 => (Int -> R -> f R) -> VecLarge d -> f (VecLarge d)
      itraverse' f (VecLarge v) = VecLarge <$> itraverse'' f v
  {-# INLINE components #-}


traverse''     :: (UV.Unbox a, UV.Unbox b, Applicative f)
               => (a -> f b) -> UV.Vector a -> f (UV.Vector b)
traverse'' f v = UV.fromListN (UV.length v) <$> traverse f (UV.toList v)

itraverse''     :: (UV.Unbox a, UV.Unbox b, Applicative f)
                => (Int -> a -> f b) -> UV.Vector a -> f (UV.Vector b)
itraverse'' f v = UV.fromListN (UV.length v) <$> itraverse f (UV.toList v)


instance KnownNat d => Vector_ (VecLarge d) d R where
  vectorFromList xs = let v = UV.fromList xs
                          d = fromIntegral . natVal $ Proxy @d
                      in if UV.length v == d then Just (VecLarge v) else Nothing
  {-# INLINE vectorFromList #-}

instance KnownNat d => Additive_ (VecLarge d) where
  zero   = let d = fromIntegral . natVal $ Proxy @d
           in VecLarge $ UV.replicate d 0
  liftU2 f (VecLarge v) (VecLarge w) = VecLarge $ UV.zipWith f v w
  liftI2 f (VecLarge v) (VecLarge w) = VecLarge $ UV.zipWith f v w

instance KnownNat d => Metric_ (VecLarge d)


--------------------------------------------------------------------------------

{-
-- | elements of the vector are stored consecutively
newtype instance UMV.MVector s (VecLarge d) = MV_VecLarge (UMV.MVector s R)
newtype instance UV.Vector     (VecLarge d) = V_VecLarge  (UV.Vector     R)

instance KnownNat d => GMV.MVector UMV.MVector (VecLarge d) where
  basicLength (MV_VecLarge v) = let d = fromIntegral . natVal $ Proxy @d
                                in GMV.basicLength v `div` d
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_VecLarge v) = let d = fromIntegral . natVal $ Proxy @d
                                         in MV_VecLarge $ GMV.basicUnsafeSlice (d*s) (d*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_VecLarge v) (MV_VecLarge v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = let d = fromIntegral . natVal $ Proxy @d
                     in MV_VecLarge <$> GMV.basicUnsafeNew (d*n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_VecLarge v) = GMV.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_VecLarge v) i = let d = fromIntegral . natVal $ Proxy @d
                                      in VecLarge . UV.unsafeFreeze
                                         <$> UMV.unsafeSlice (d*i) d v
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_VecLarge v) i (VecLarge w) = let d = fromIntegral . natVal $ Proxy @d
                                                    in GMV.iforM_ w $ \j x ->
                                                         GMV.basicUnsafeWrite v (d*i+j) x
  {-# INLINE basicUnsafeWrite #-}


-- type instance GV.Mutable UV.Vector2

instance KnownNat d => GV.Vector UV.Vector (VecLarge d) where

  basicUnsafeFreeze (MV_VecLarge mv) = V_VecLarge <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_VecLarge v) = MV_VecLarge <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_VecLarge v) = let d = fromIntegral . natVal $ Proxy @d
                               in GV.basicLength v `div` d
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_VecLarge v) = let d = fromIntegral . natVal $ Proxy @d
                                        in V_VecLarge $ GV.basicUnsafeSlice (d*s) (d*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_VecLarge v) i = let d = fromIntegral . natVal $ Proxy @d
                                       in VecLarge <$> UV.unsafeSlice (d*i) d v
  {-# INLINE basicUnsafeIndexM #-}

    -- let d = fromIntegral . natVal $ Proxy @d
    --                                    in do

    -- VecLarge <$>

    -- Vec2 <$> GV.basicUnsafeIndexM v (2*i)
    --                                     <*> GV.basicUnsafeIndexM v (2*i+1)

instance KnownNat d => UV.Unbox (VecLarge d)
-}
