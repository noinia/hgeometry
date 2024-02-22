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
import           HGeometry.Vector
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))
--import HGeometry.Point.EuclideanDistance
import           Text.Read (Read (..), readListPrecDefault)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
-- import           Data.Zip

--------------------------------------------------------------------------------

-- | A Point wraps a vector
newtype PointF v = Point { toVec :: v }
                 deriving stock   (Generic)
                 deriving newtype (Eq, Ord, Random, NFData, Bounded, Enum)
-- don't derive functor, or so here. since that will be confusing.

type instance Dimension (PointF v) = Dimension v
type instance NumType   (PointF v) = IxValue v
type instance IxValue   (PointF v) = IxValue v

-- deriving newtype instance Functor v => Functor (PointF v)
-- deriving newtype instance Foldable v => Foldable (PointF v)
-- deriving newtype instance Traversable v => Traversable (PointF v)

-- Functor, Foldable, Traversable

_PointF :: Iso (PointF v) (PointF v') v v'
_PointF = iso toVec Point
{-# INLINE _PointF #-}

instance ( Additive_ vector       d r
         , Additive_ (Vector d r) d r
         , Show r
         ) => Show (PointF vector) where
  showsPrec k p = showParen (k > app_prec) $
                    showString constr . showChar ' ' .
                    unwordsS (map (showsPrec 11) (p^..coordinates))
    where
      app_prec = 10
      constr   = "Point" <> show (fromIntegral (natVal @d Proxy))
      unwordsS = foldr (.) id . intersperse (showChar ' ')

instance ( Additive_ vector       d r
         , Additive_ (Vector d r) d r
         , Read r
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

instance ( Vector_ vector d r
         , Vector_ vector' d s
         , Has_ Vector_ d r
         , Has_ Vector_ d s
         , AsVector_ vector vector' d r s
         , HasComponents vector vector'
         ) => HasVector (PointF vector) (PointF vector') where

  vector = _PointF._Vector
  {-# INLINE vector #-}

instance ( Has_ Vector_ d r
         , Has_ Vector_ d s
         , Vector_ vector d r
         , Vector_ vector' d s
         , AsVector_ vector vector' d r s
         , HasComponents (Vector d r) (Vector d s)
         , HasComponents vector vector'
         ) => HasCoordinates (PointF vector) (PointF vector')

instance ( Additive_ vector       d r
         , Additive_ (Vector d r) d r
         ) => Affine_ (PointF vector) d r where

instance ( Additive_ vector       d r
         , Additive_ (Vector d r) d r
         ) => Point_ (PointF vector) d r where
  fromVector = Point . review _Vector
  {-# INLINE fromVector #-}

instance HasPoints (PointF v) (PointF v') (PointF v) (PointF v') where
  allPoints = id

instance Uniform v => Uniform (PointF v) where
  uniformM gen = Point <$> uniformM gen

instance (UniformRange v) => UniformRange (PointF v) where
  uniformRM (Point lows, Point highs) gen = Point <$> uniformRM (lows,highs) gen

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
