--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Point.PointF
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implements a point by wrapping some Vector type
--
--------------------------------------------------------------------------------
module HGeometry.Point.PointF
  ( PointF(..)
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad (replicateM)
import           Data.Aeson
import           Data.Functor.Classes
import           Data.List (intersperse)
import           Data.Proxy
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           HGeometry.Point.Class
import           HGeometry.Properties
import           HGeometry.Vector
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))
--import HGeometry.Point.EuclideanDistance
import           Text.Read (Read (..), readListPrecDefault)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector.Unboxed as UV

--------------------------------------------------------------------------------

-- | A Point wraps a vector
newtype PointF v = Point { toVec :: v }
                 deriving ( Eq, Ord, Generic
                          , Functor, Foldable, Traversable
                          , NFData, Bounded, Enum, Random
                          , ToJSON, FromJSON -- not sure we want these like this
                          )

type instance Dimension (PointF v) = Dimension v
type instance NumType   (PointF v) = NumType v
type instance VectorFor (PointF v) = v


instance ( Show (IxValue v)
         , KnownNat (Dimension v)
         , NumType v ~ IxValue v
         ) => Show (PointF v) where
  showsPrec k p = showParen (k > app_prec) $
                    showString constr . showChar ' ' .
                    unwordsS (map (showsPrec 11) (p^..coordinates))
    where
      app_prec = 10
      constr   = "Point" <> show (fromIntegral (natVal @(Dimension v) Proxy))
      unwordsS = foldr (.) id . intersperse (showChar ' ')

instance ( Read (IxValue v)
         , KnownNat (Dimension v)
         , IxValue v ~ NumType v
         ) => Read (PointF v) where
  readPrec = readData $
      readUnaryWith (replicateM d readPrec) constr $ \rs ->
        case pointFromList rs of
          Just p -> p
          _      -> error "internal error in HGeometry.Point read instance."
    where
      d        = fromIntegral (natVal @(Dimension v) Proxy)
      constr   = "Point" <> show d
  readListPrec = readListPrecDefault


p .-. q = toVec p ^-^ toVec q

p .+^ v = Point $ toVec p ^+^ v

p .-^ v = Point $ toVec p ^-^ v

vector = lens toVec (const Point)


-- instance HasPoints (PointF v) (PointF v') (PointF v) (PointF v') where
--   allPoints = id

instance Uniform v => Uniform (PointF v) where
  uniformM gen = Point <$> uniformM gen

instance (UniformRange v) => UniformRange (PointF v) where
  uniformRM (Point lows, Point highs) gen = Point <$> uniformRM (lows,highs) gen


-- -- | use the optimal representation for v.
-- type instance VectorFamily d (PointF v) = WrapVector d v (PointF v)


-- newtype instance UMV.MVector s (PointF v) = MV_PointF (UMV.MVector s v)
-- newtype instance UV.Vector     (PointF v) = V_PointF  (UV.Vector     v)


-- -- deriving instance GMV.MVector UMV.MVector v => GMV.MVector UMV.MVector (PointF v)
-- -- instance UV.Unbox v => UV.Unbox (PointF v)

-- instance GMV.MVector UMV.MVector v => GMV.MVector UMV.MVector (PointF v) where
--   basicLength (MV_PointF v) = GMV.basicLength v
--   {-# INLINE basicLength #-}
--   basicUnsafeSlice s l (MV_PointF v) = MV_PointF $ GMV.basicUnsafeSlice s l v
--   {-# INLINE basicUnsafeSlice #-}
--   basicOverlaps  (MV_PointF v) (MV_PointF v') = GMV.basicOverlaps v v'
--   {-# INLINE basicOverlaps #-}
--   basicUnsafeNew n = MV_PointF <$> GMV.basicUnsafeNew n
--   {-# INLINE basicUnsafeNew #-}
--   basicInitialize (MV_PointF v) = GMV.basicInitialize v
--   {-# INLINE basicInitialize#-}
--   basicUnsafeRead (MV_PointF v) i = Point <$> GMV.basicUnsafeRead v i
--   {-# INLINE basicUnsafeRead #-}
--   basicUnsafeWrite (MV_PointF v) i (Point p) = GMV.basicUnsafeWrite v i p
--   {-# INLINE basicUnsafeWrite #-}

-- instance GV.Vector UV.Vector v => GV.Vector UV.Vector (PointF v) where

--   basicUnsafeFreeze (MV_PointF mv) = V_PointF <$> GV.basicUnsafeFreeze mv
--   {-# INLINE basicUnsafeFreeze #-}
--   basicUnsafeThaw (V_PointF v) = MV_PointF <$> GV.basicUnsafeThaw v
--   {-# INLINE basicUnsafeThaw #-}
--   basicLength (V_PointF v) = GV.basicLength v
--   {-# INLINE basicLength #-}
--   basicUnsafeSlice s l (V_PointF v) = V_PointF $ GV.basicUnsafeSlice s l v
--   {-# INLINE basicUnsafeSlice #-}
--   basicUnsafeIndexM (V_PointF v) i = Point <$> GV.basicUnsafeIndexM v i
--   {-# INLINE basicUnsafeIndexM #-}

-- instance UV.Unbox v => UV.Unbox (PointF v)
