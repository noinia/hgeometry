{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Point.PointF
  ( PointF(..)
  ) where

import Control.Lens
import Control.Monad (replicateM)
import Data.Aeson
import Data.Functor.Classes
import Data.List (intersperse)
import Data.Proxy
import GHC.Generics (Generic)
import GHC.TypeLits
import HGeometry.Point.Class
import HGeometry.Properties
import HGeometry.Vector.Class
import System.Random.Stateful (UniformRange(..), Uniform(..))
import Control.DeepSeq
import System.Random (Random (..))
--import HGeometry.Point.EuclideanDistance
import Text.Read (Read (..), readListPrecDefault)

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


instance ( Vector_ v (Dimension v) (NumType v)
         , Metric_ v
         , Show (NumType v)
         , KnownNat (Dimension v)
         ) => Show (PointF v) where
  showsPrec k p = showParen (k > app_prec) $
                    showString constr . showChar ' ' .
                    unwordsS (map (showsPrec 11) (p^..coordinates))
    where
      app_prec = 10
      constr   = "Point" <> show (fromIntegral (natVal @(Dimension v) Proxy))
      unwordsS = foldr (.) id . intersperse (showChar ' ')

instance ( Vector_ v (Dimension v) (NumType v)
         , Metric_ v
         , Read (NumType v)
         , KnownNat (Dimension v)
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

instance ( Vector_ v (Dimension v) (NumType v)
         , Metric_ v
         ) => Affine_ (PointF v) where
  p .-. q = toVec p ^-^ toVec q
  p .+^ v = Point $ toVec p ^+^ v
  p .-^ v = Point $ toVec p ^-^ v

instance ( Vector_ v  d r
         , Vector_ v' d s
         ) => HasVector (PointF v) (PointF v') r s where
  vector = lens toVec (const Point)

instance ( Vector_ v d r
         , Metric_ v
         ) => Point_ (PointF v) d r where
  fromVector = Point . vectorFromVector

instance HasPoints (PointF v) (PointF v') (PointF v) (PointF v') where
  allPoints = id

-- instance Num (NumType v) => HasSquaredEuclideanDistance (PointF v) where
--   pointClosestTo _ = id

instance Uniform v => Uniform (PointF v) where
  uniformM gen = Point <$> uniformM gen

instance (UniformRange v) => UniformRange (PointF v) where
  uniformRM (Point lows, Point highs) gen = Point <$> uniformRM (lows,highs) gen
