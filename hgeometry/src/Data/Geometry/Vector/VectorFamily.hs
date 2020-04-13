{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Vector.VectorFamily
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of \(d\)-dimensional vectors. The implementation
-- automatically selects an optimized representation for small (up to size 4)
-- vectors.
--
--------------------------------------------------------------------------------
module Data.Geometry.Vector.VectorFamily where

import           Control.DeepSeq
import           Control.Lens hiding (element)
import           Data.Aeson
-- import           Data.Aeson (ToJSON(..),FromJSON(..))
import qualified Data.Foldable as F
import qualified Data.List as L
import           Data.Geometry.Vector.VectorFixed (C(..))
import qualified Data.Geometry.Vector.VectorFamilyPeano as Fam
import           Data.Geometry.Vector.VectorFamilyPeano ( VectorFamily(..)
                                                        , VectorFamilyF
                                                        , ImplicitArity
                                                        )
import qualified Data.Vector.Fixed as V
import           Data.Vector.Fixed.Cont (Peano)
import           GHC.TypeLits
import           Linear.Affine (Affine(..))
import           Linear.Metric
import qualified Linear.V2 as L2
import qualified Linear.V3 as L3
import qualified Linear.V4 as L4
import           Linear.Vector
import           Text.ParserCombinators.ReadP (ReadP, string,pfail)
import           Text.ParserCombinators.ReadPrec (lift)
import           Text.Read (Read(..),readListPrecDefault, readPrec_to_P,minPrec)
import           Data.Proxy
import           Data.Hashable

--------------------------------------------------------------------------------
-- * d dimensional Vectors


-- | Datatype representing d dimensional vectors. The default implementation is
-- based n VectorFixed. However, for small vectors we automatically select a
-- more efficient representation.
newtype Vector (d :: Nat) (r :: *) = MKVector { _unV :: VectorFamily (Peano d) r }

type instance V.Dim   (Vector d)   = Fam.FromPeano (Peano d)
-- the above definition is a bit convoluted, but it allows us to make Vector an instance of
-- V.Vector having only an Arity constraint rather than an Arity2 constraint.
type instance Index   (Vector d r) = Int
type instance IxValue (Vector d r) = r

unV :: Lens (Vector d r) (Vector d s) (VectorFamily (Peano d) r) (VectorFamily (Peano d) s)
unV = lens _unV (const MKVector)
{-# INLINE unV #-}

-- type Arity d = (ImplicitArity (Peano d), KnownNat d)
class (ImplicitArity (Peano d), KnownNat d) => Arity d
instance (ImplicitArity (Peano d), KnownNat d) => Arity d


deriving instance (Eq r,  Arity d) => Eq  (Vector d r)
deriving instance (Ord r, Arity d) => Ord (Vector d r)

deriving instance Arity d => Functor     (Vector d)
deriving instance Arity d => Foldable    (Vector d)
deriving instance Arity d => Traversable (Vector d)
deriving instance Arity d => Applicative (Vector d)

deriving instance Arity d => Additive (Vector d)
deriving instance Arity d => Metric (Vector d)
instance Arity d => Affine (Vector d) where
  type Diff (Vector d) = Vector d
  u .-. v = u ^-^ v
  p .+^ v = p ^+^ v

deriving instance (Arity d, Hashable r) => Hashable (Vector d r)

instance Arity d => Ixed (Vector d r) where
  ix = element'

instance Arity d => V.Vector (Vector d) r where
  construct  = MKVector <$> V.construct
  inspect    = V.inspect . _unV
  basicIndex = V.basicIndex . _unV

instance (Arity d, Show r) => Show (Vector d r) where
  show v = mconcat [ "Vector", show $ F.length v , " "
                   , show $ F.toList v ]

instance (Read r, Arity d) => Read (Vector d r) where
  readPrec     = lift readVec
  readListPrec = readListPrecDefault

readVec :: forall d r. (Arity d, Read r) => ReadP (Vector d r)
readVec = do let d = natVal (Proxy :: Proxy d)
             _  <- string $ "Vector" <> show d <> " "
             rs <- readPrec_to_P readPrec minPrec
             case vectorFromList rs of
               Just v -> pure v
               _      -> pfail

deriving instance (FromJSON r, Arity d) => FromJSON (Vector d r)
instance (ToJSON r, Arity d) => ToJSON (Vector d r) where
  toJSON     = toJSON . _unV
  toEncoding = toEncoding . _unV

deriving instance (NFData r, Arity d) => NFData (Vector d r)

--------------------------------------------------------------------------------
-- * Convenience "constructors"

pattern Vector   :: VectorFamilyF (Peano d) r -> Vector d r
pattern Vector v = MKVector (VectorFamily v)
{-# COMPLETE Vector #-}

pattern Vector1   :: r -> Vector 1 r
pattern Vector1 x = (Vector (Identity x))
{-# COMPLETE Vector1 #-}

pattern Vector2     :: r -> r -> Vector 2 r
pattern Vector2 x y = (Vector (L2.V2 x y))
{-# COMPLETE Vector2 #-}

pattern Vector3        :: r -> r -> r -> Vector 3 r
pattern Vector3 x y z  = (Vector (L3.V3 x y z))
{-# COMPLETE Vector3 #-}

pattern Vector4         :: r -> r -> r -> r -> Vector 4 r
pattern Vector4 x y z w = (Vector (L4.V4 x y z w))
{-# COMPLETE Vector4 #-}

--------------------------------------------------------------------------------

vectorFromList :: Arity d => [r] -> Maybe (Vector d r)
vectorFromList = V.fromListM

vectorFromListUnsafe :: Arity d => [r] -> Vector d r
vectorFromListUnsafe = V.fromList

destruct   :: (Arity d, Arity (d + 1))
           => Vector (d + 1) r -> (r, Vector d r)
destruct v = (L.head $ F.toList v, vectorFromListUnsafe . tail $ F.toList v)
  -- FIXME: this implementaion of tail is not particularly nice

head   :: (Arity d, 1 <= d) => Vector d r -> r
head = view $ element (C :: C 0)

--------------------------------------------------------------------------------
-- * Indexing vectors

-- | Lens into the i th element
element   :: forall proxy i d r. (Arity d, KnownNat i, (i + 1) <= d)
          => proxy i -> Lens' (Vector d r) r
element _ = singular . element' . fromInteger $ natVal (C :: C i)
{-# INLINE element #-}


-- | Similar to 'element' above. Except that we don't have a static guarantee
-- that the index is in bounds. Hence, we can only return a Traversal
element' :: forall d r. Arity d => Int -> Traversal' (Vector d r) r
element' i = unV.(e (C :: C d) i)
  where
    e  :: Arity d => proxy d -> Int -> Traversal' (VectorFamily (Peano d) r) r
    e _ = Fam.element'
{-# INLINE element' #-}

--------------------------------------------------------------------------------
-- * Snoccing and consindg

-- | Add an element at the back of the vector
snoc     :: (Arity (d + 1), Arity d) => Vector d r -> r -> Vector (d + 1) r
snoc v x = vectorFromListUnsafe . (++ [x]) $ F.toList v
  -- FIXME: horrible implementation here as well

-- | Get a vector of the first d - 1 elements.
init :: (Arity d, Arity (d + 1)) => Vector (d + 1) r -> Vector d r
init = vectorFromListUnsafe . L.init . F.toList

last :: forall d r. (KnownNat d, Arity (d + 1)) => Vector (d + 1) r -> r
last = view $ element (C :: C d)

-- | Get a prefix of i elements of a vector
prefix :: forall i d r. (Arity d, Arity i, i <= d)
       => Vector d r -> Vector i r
prefix = let i = fromInteger . natVal $ (C :: C i)
         in vectorFromListUnsafe . take i . F.toList

--------------------------------------------------------------------------------
-- * Specific on 3-dimensional vectors
-- | Cross product of two three-dimensional vectors
cross       :: Num r => Vector 3 r -> Vector 3 r -> Vector 3 r
(Vector u) `cross` (Vector v) = Vector $ u `L3.cross` v
