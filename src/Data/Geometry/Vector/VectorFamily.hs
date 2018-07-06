{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector.VectorFamily where

import           Control.DeepSeq
import           Control.Lens hiding (element)
-- import           Data.Aeson (ToJSON(..),FromJSON(..))
import qualified Data.Foldable as F
import           Data.Geometry.Vector.VectorFixed(C(..))
import qualified Data.Geometry.Vector.VectorFamilyPeano as Fam
import           Data.Geometry.Vector.VectorFamilyPeano ( VectorFamily(..)
                                                        , VectorFamilyF
                                                        , ImplicitArity
                                                        )
import           Data.Semigroup
import qualified Data.Vector.Fixed as V
import           Data.Vector.Fixed.Cont (Peano, PeanoNum(..), Fun(..))
import           GHC.TypeLits
import           Linear.Affine (Affine(..))
import           Linear.Metric
import qualified Linear.V2 as L2
import qualified Linear.V3 as L3
import qualified Linear.V4 as L4
import           Linear.Vector

--------------------------------------------------------------------------------
-- * d dimensional Vectors


-- | Datatype representing d dimensional vectors. The default implementation is
-- based n VectorFixed. However, for small vectors we automatically select a
-- more efficient representation.
newtype Vector (d :: Nat) (r :: *) = MKVector { _unV :: VectorFamily (Peano d) r }

type instance V.Dim   (Vector d)   = d
type instance Index   (Vector d r) = Int
type instance IxValue (Vector d r) = r

type Arity d = ( ImplicitArity (Peano d)
               , V.Arity d
               , Fam.FromPeano (Peano d) ~ d
               )

deriving instance (Eq r,  Arity d) => Eq  (Vector d r)
deriving instance (Ord r, Arity d) => Ord (Vector d r)

deriving instance Arity d => Functor     (Vector d)
deriving instance Arity d => Foldable    (Vector d)
deriving instance Arity d => Traversable (Vector d)

deriving instance Arity d => Additive (Vector d)
deriving instance Arity d => Metric (Vector d)
deriving instance Arity d => Affine (Vector d)

instance Arity d => Ixed (Vector d r) where
  ix = element'

instance Arity d => V.Vector (Vector d) r where
  construct  = MKVector <$> V.construct
  inspect    = V.inspect . _unV
  basicIndex = V.basicIndex . _unV

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

vectorFromList :: Arity d => [r] -> Maybe (Vector d r)
vectorFromList = V.fromListM

vectorFromListUnsafe :: Arity d => [r] -> Vector d r
vectorFromListUnsafe = V.fromList




-- destruct            :: (Arity d, Arity (d + 1), 1 <= (d + 1))
--                     => Vector (d + 1) r -> (r, Vector d r)
-- destruct (Vector v) = (V.head v, V.tail v)

--------------------------------------------------------------------------------
-- * Indexing vectors

-- | Lens into the i th element
element   :: forall proxy i d r. (Arity d, Arity i, (i + 1) <= d)
          => proxy i -> Lens' (Vector d r) r
element _ = singular . element' . fromInteger $ natVal (C :: C i)


-- | Similar to 'element' above. Except that we don't have a static guarantee
-- that the index is in bounds. Hence, we can only return a Traversal
element' :: forall d r. Arity d => Int -> Traversal' (Vector d r) r
element' = V.element

--------------------------------------------------------------------------------

-- | Cross product of two three-dimensional vectors
cross       :: Num r => Vector 3 r -> Vector 3 r -> Vector 3 r
(Vector u) `cross` (Vector v) = Vector $ u `L3.cross` v
