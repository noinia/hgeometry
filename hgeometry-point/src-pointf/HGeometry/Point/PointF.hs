{-# LANGUAGE UndecidableInstances #-}
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
-- import           Data.Aeson
import           Data.Functor.Classes
import           Data.List (intersperse)
import           Data.Proxy
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           HGeometry.Point.Class
import           HGeometry.Properties
import           HGeometry.Vector.Class
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))
--import HGeometry.Point.EuclideanDistance
import           Text.Read (Read (..), readListPrecDefault)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

--------------------------------------------------------------------------------

-- | A Point wraps a vector
newtype PointF v = Point { toVec :: v }
                 deriving stock   (Generic, Functor, Foldable, Traversable)
                 deriving newtype (Eq, Ord, Random, NFData, Bounded, Enum)

type instance Dimension (PointF v) = Dimension v
type instance NumType   (PointF v) = NumType v
type instance IxValue   (PointF v) = IxValue v

-- type instance VectorFor (PointF v) = v

_PointF :: Iso (PointF v) (PointF v') v v'
_PointF = iso toVec Point



instance ( d ~ Dimension vector
         , r ~ NumType vector
         , r ~ IxValue vector
         , Vector_ vector
         , Additive_ (Vector d r)
         , Show r
         , KnownNat d
         ) => Show (PointF vector) where
  showsPrec k p = showParen (k > app_prec) $
                    showString constr . showChar ' ' .
                    unwordsS (map (showsPrec 11) (p^..coordinates))
    where
      app_prec = 10
      constr   = "Point" <> show (fromIntegral (natVal @d Proxy))
      unwordsS = foldr (.) id . intersperse (showChar ' ')

instance ( d ~ Dimension vector
         , r ~ NumType vector
         , r ~ IxValue vector
         , Vector_ vector
         , Additive_ (Vector d r)
         , Read r
         , KnownNat d
         ) => Read (PointF vector) where
  readPrec = readData $
      readUnaryWith (replicateM d readPrec) constr $ \rs ->
        case pointFromList rs of
          Just p -> p
          _      -> error "internal error in HGeometry.Point read instance."
    where
      d        = fromIntegral (natVal @d Proxy)
      constr   = "Point" <> show d
  readListPrec = readListPrecDefault

-- instance ( Vector_ v (Dimension v) (IxValue v)
--          , Metric_ v
--          , IxValue v ~ NumType v
--          ) => Affine_ (PointF v) where
--   p .-. q = toVec p ^-^ toVec q
--   p .+^ v = Point $ toVec p ^+^ v
--   p .-^ v = Point $ toVec p ^-^ v

instance ( Vector_ vector
         , IxValue vector ~ NumType vector
         ) => HasVector (PointF vector) where
  {-# SPECIALIZE instance HasVector (PointF (Vector d r)) #-}
  vector = _PointF._Vector

instance ( d ~ Dimension vector
         , r ~ NumType vector
         , r ~ IxValue vector
         , Vector_ vector
         , Additive_ (Vector d r)
         ) => Affine_ (PointF vector) d r where
  {-# SPECIALIZE instance Additive_ (Vector d r) => Affine_ (PointF (Vector d r)) d r #-}

instance ( d ~ Dimension vector
         , r ~ NumType vector
         , r ~ IxValue vector
         , Vector_ vector
         , Additive_ (Vector d r)
         ) => Point_ (PointF vector) d r where
  {-# SPECIALIZE instance Additive_ (Vector d r) => Point_ (PointF (Vector d r)) d r #-}
  fromVector = Point . review _Vector

instance HasPoints (PointF v) (PointF v') (PointF v) (PointF v') where
  allPoints = id

instance Uniform v => Uniform (PointF v) where
  uniformM gen = Point <$> uniformM gen

instance (UniformRange v) => UniformRange (PointF v) where
  uniformRM (Point lows, Point highs) gen = Point <$> uniformRM (lows,highs) gen



-- -- | use the optimal representation for v.
-- type instance VectorFamily d (PointF v) = WrapVector d v (PointF v)


newtype instance U.MVector s (PointF v) = MV_PointF (U.MVector s v)
newtype instance U.Vector    (PointF v) = V_PointF  (U.Vector    v)

instance U.IsoUnbox (PointF v) v where
  toURepr (Point v) = v
  fromURepr = Point
  {-# INLINE toURepr #-}
  {-# INLINE fromURepr #-}

deriving via ((PointF v) `U.As` v) instance U.Unbox v => GM.MVector U.MVector (PointF v)
deriving via ((PointF v) `U.As` v) instance U.Unbox v => G.Vector   U.Vector  (PointF v)
instance U.Unbox v => U.Unbox (PointF v)
