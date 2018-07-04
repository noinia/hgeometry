{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector.VectorFamily where

import           Control.Applicative (liftA2)
import           Control.DeepSeq
import           Control.Lens hiding (element)
-- import           Data.Aeson (ToJSON(..),FromJSON(..))
import qualified Data.Foldable as F
import qualified Data.Geometry.Vector.VectorFixed as FV
import qualified Data.Geometry.Vector.VectorFamilyPeano as Fam
import           Data.Geometry.Vector.VectorFamilyPeano (VectorFamily, Arity)
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Semigroup
import           Data.Traversable (foldMapDefault,fmapDefault)
import qualified Data.Vector.Fixed as V
import           Data.Vector.Fixed.Cont (Peano(..), PeanoNum(..), Fun(..))
import           GHC.TypeLits
import           Linear.Affine (Affine(..))
import           Linear.Metric
import qualified Linear.V2 as L2
import qualified Linear.V3 as L3
import qualified Linear.V4 as L4
import           Linear.Vector

--------------------------------------------------------------------------------
-- * d dimensional Vectors


newtype Vector (d :: Nat) (r :: *) = MKVector { _unV :: VectorFamily (Peano d) r }

type instance V.Dim (Vector d)  = d


type instance Index   (Vector d r) = Int
type instance IxValue (Vector d r) = r

type Arity d = ImplicitArity (Peano d)

deriving instance (Eq r,  Arity d) => Eq  (Vector d r)
deriving instance (Ord r, Arity d) => Ord (Vector d r)

deriving instance Arity d => Functor     (Vector d)
deriving instance Arity d => Foldable    (Vector d)
deriving instance Arity d => Traversable (Vector d)

instance (Arity d, Show r) => Show (Vector d r) where
  show v = mconcat [ "Vector", show $ F.length v , " "
                   , show $ F.toList v ]


deriving instance (NFData (VectorFamily (Peano d) r)) => NFData (Vector d r)

--------------------------------------------------------------------------------
-- * Convenience "constructors"

pattern Vector   :: VectorFamilyF (Peano d) r -> Vector d r
pattern Vector v = MKVector (VectorFamily v)

pattern Vector1   :: r -> Vector 1 r
pattern Vector1 x = (Vector (Identity x))

pattern Vector2     :: r -> r -> Vector 2 r
pattern Vector2 x y = (Vector (L2.V2 x y))

pattern Vector3        :: r -> r -> r -> Vector 3 r
pattern Vector3 x y z  = (Vector (L3.V3 x y z))

pattern Vector4         :: r -> r -> r -> r -> Vector 4 r
pattern Vector4 x y z w = (Vector (L4.V4 x y z w))

--------------------------------------------------------------------------------

vectorFromList :: Arity d => [a] -> Maybe (Vector d a)
vectorFromList = fmap Vector . Fam.vectorFromList

vectorFromListUnsafe :: Arity d => [a] -> Vector d a
vectorFromListUnsafe = Vector . Fam.vectorFromListUnsafe


-- -- destruct            :: (Vec d r, Vec (d + 1) r, 1 <= (d + 1))
-- --                     => Vector (d + 1) r -> (r, Vector d r)
-- -- destruct (Vector v) = (V.head v, Vector $ V.tail v)


-- -- -- vectorFromList :: Arity d => [a] -> Maybe (Vector d a)
-- -- vectorFromList = fmap Vector . V.fromListM

-- -- vectorFromListUnsafe :: V.Arity d => [a] -> Vector d a
-- -- vectorFromListUnsafe = Vector . V.fromList

 --------------------------------------------------------------------------------

-- | Cross product of two three-dimensional vectors
cross       :: Num r => Vector 3 r -> Vector 3 r -> Vector 3 r
(Vector u) `cross` (Vector v) = Vector $ u `L3.cross` v
