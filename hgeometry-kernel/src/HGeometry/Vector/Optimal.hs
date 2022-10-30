{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Vector.Optimal
  ( Vector(Vector1, Vector2, Vector3, Vector4)
  , V2.HasV2(..)
  , V3.HasV3(..)
  , V4.HasV4(..)
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Data.Coerce
import           GHC.Generics
import           GHC.TypeLits
import           HGeometry.Properties
import           HGeometry.Vector.Class
import           HGeometry.Vector.Helper
import qualified HGeometry.Vector.Optimal.Large as Large
import qualified HGeometry.Vector.Optimal.V1 as V1
import qualified HGeometry.Vector.Optimal.V2 as V2
import qualified HGeometry.Vector.Optimal.V3 as V3
import qualified HGeometry.Vector.Optimal.V4 as V4
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))
import           Text.Read (Read (..), readListPrecDefault)

--------------------------------------------------------------------------------

-- | d-dimensional vectors that pick a specialized implementation for sizes up to 4.
newtype Vector d r = MkVector (VectorImpl d r)

type instance Dimension (Vector d r) = Dimension (VectorImpl d r)
type instance NumType  (Vector d r)  = NumType (VectorImpl d r)
type instance IxValue  (Vector d r)  = IxValue (VectorImpl d r)
type instance Index    (Vector d r)  = Index   (VectorImpl d r)

type family VectorImpl d r where
  VectorImpl 0 r = Const () r
  VectorImpl 1 r = V1.V1 r
  VectorImpl 2 r = V2.V2 r
  VectorImpl 3 r = V3.V3 r
  VectorImpl 4 r = V4.V4 r
  VectorImpl d r = Large.LargeVector d r

--------------------------------------------------------------------------------

pattern Vector1   :: r -> Vector 1 r
pattern Vector1 x = MkVector (V1.Vector1 x)

pattern Vector2     :: V2.HasV2 r => r -> r -> Vector 2 r
pattern Vector2 x y = MkVector (V2.Vector2 x y)

pattern Vector3   :: V3.HasV3 r => r -> r -> r -> Vector 3 r
pattern Vector3 x y z = MkVector (V3.Vector3 x y z)

pattern Vector4         :: V4.HasV4 r => r -> r -> r -> r -> Vector 4 r
pattern Vector4 x y z w = MkVector (V4.Vector4 x y z w)

--------------------------------------------------------------------------------

deriving newtype instance Eq (VectorImpl d r)      => Eq (Vector d r)
deriving newtype instance Ord (VectorImpl d r)     => Ord (Vector d r)
deriving newtype instance NFData (VectorImpl d r)  => NFData (Vector d r)
deriving newtype instance Generic (VectorImpl d r) => Generic (Vector d r)

instance Ixed (VectorImpl d r) => Ixed (Vector d r) where
  ix i f (MkVector v) = MkVector <$> ix i f v


instance {-# OVERLAPPING #-}
         HasComponents (VectorImpl d r) (VectorImpl d s)
      => HasComponents (Vector d r)     (Vector d s) where
  components f (MkVector v) = MkVector <$> components f v

instance HasComponents (VectorImpl d r) v => HasComponents (Vector d r) v where
  components f (MkVector v) = components f v

instance Vector_ (VectorImpl d r) d r => Vector_ (Vector d r) d r where
  vectorFromList = fmap MkVector . vectorFromList @(VectorImpl d r)

deriving instance Additive_ (VectorImpl d r) => Additive_ (Vector d r)
deriving instance Metric_ (VectorImpl d r)   => Metric_   (Vector d r)



instance ( Show r, KnownNat d, Vector_ (Vector d r) d r
         ) => Show (Vector d r) where
  showsPrec = showsPrecVec
instance ( Read r, KnownNat d, Vector_ (Vector d r) d r
         ) => Read (Vector d r) where
  readPrec = readPrecVec
  readListPrec = readListPrecDefault


deriving instance Random (VectorImpl d r) => Random (Vector d r)

instance UniformRange (VectorImpl d r) => UniformRange (Vector d r) where
  uniformRM rng gen = MkVector <$> uniformRM (coerce rng) gen

instance Uniform (VectorImpl d r) => Uniform (Vector d r) where
  uniformM gen = MkVector <$> uniformM gen
