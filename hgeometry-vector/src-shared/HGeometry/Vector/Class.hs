--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Clas
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- typeclass that expresses that we can essentially add vectors
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module HGeometry.Vector.Class
  ( Vector
  , VectorLike_(..)
  , component
  -- , Additive_(..), negated, (*^), (^*), (^/), sumV, basis, unit
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           Data.Kind (Type)
import qualified Data.List as List
import           Data.Proxy
import           Data.Type.Ord
import           GHC.TypeLits (Nat, KnownNat, natVal)
import           HGeometry.Properties
import qualified Linear.V1
import qualified Linear.V2
import qualified Linear.V3
import qualified Linear.V4

--------------------------------------------------------------------------------

-- | d-dimensional vectors
data family Vector (d :: Nat) (r :: Type) :: Type

type instance IxValue   (Vector d r) = r
type instance Dimension (Vector d r) = d

--------------------------------------------------------------------------------

-- | Types that have a 'components' indexed traversal
class VectorLike_ vector where
  -- | An Indexed Traversal over the components of a vector
  components :: IndexedTraversal1' Int vector (IxValue vector)
  -- | Access the i^th component. Consider using 'component' instead.
  unsafeComponent  :: Int -> IndexedLens' Int vector (IxValue vector)

-- | Lens to access te i^t component.
--
-- >>> myVec3 ^. component @0
-- 1
-- >>> myVec3 ^. component @1
-- 2
-- >>> myVec3 & component @1 %~ (*5)
-- Vector3 1 10 3
-- >>> myVec2 & component @1 %~ (*5)
-- Vector2 10 100
component :: forall i vector. (VectorLike_ vector, i < Dimension vector, KnownNat i)
          => IndexedLens' Int vector (IxValue vector)
component = unsafeComponent (fromInteger . natVal $ Proxy @i)
{-# INLINE component #-}


instance ( VectorLike_ (Vector d r)
         , Show r
         , KnownNat d
         ) => Show (Vector d r) where
  -- | Show implementation for vectors
  showsPrec k v = showParen (k > app_prec) $
                     showString constr . showChar ' ' .
                     unwordsS (map (showsPrec 11) (v^..components))
    where
      app_prec = 10
      constr   = "Vector" <> show (fromIntegral (natVal @d Proxy))
      unwordsS = foldr (.) id . List.intersperse (showChar ' ')



--------------------------------------------------------------------------------

infixl 6 ^+^, ^-^
infixl 7 ^*, *^, ^/

-- | Basically a copy of the Linear.Additive class
class VectorLike_ vector => Additive_ vector where
  {-# MINIMAL zero, liftU2, liftI2 #-}

  -- | zero vector
  zero :: Num (IxValue vector) => vector

  -- | add two vectors
  (^+^) :: Num (IxValue vector) => vector -> vector -> vector
  u ^+^ v = liftU2 (+) u v
  {-# INLINE (^+^) #-}

  -- | subtract vectors
  (^-^)   :: Num (IxValue vector) => vector -> vector -> vector
  u ^-^ v = u ^+^ negated v
  {-# INLINE (^-^) #-}

  -- | Linearly interpolate between the two vectors
  lerp           :: Num (IxValue vector) => IxValue vector -> vector -> vector -> vector
  lerp alpha u v = alpha *^ u ^+^ (1-alpha) *^ v
  {-# INLINE lerp #-}

  -- | Apply a function to merge the 'non-zero' components of two
  -- vectors, unioning the rest of the values.
  liftU2       :: (IxValue vector -> IxValue vector -> IxValue vector)
               -> vector -> vector -> vector

  -- | Apply a function to the components of two vectors.
  liftI2 :: (IxValue vector -> IxValue vector -> IxValue vector) -> vector -> vector -> vector


-- | unit vector
unit :: forall vector. (Additive_ vector, Num (IxValue vector)) => vector
unit = over components (const 1) (zero @vector)
{-# INLINABLE unit #-}

-- | negate v
negated :: (Num (IxValue vector), VectorLike_ vector) => vector -> vector
negated = ((-1) *^)
{-# INLINABLE negated #-}

-- | left scalar multiplication
(*^)   :: (Num (IxValue vector), VectorLike_ vector) => IxValue vector -> vector -> vector
s *^ v = over components (s*) v
{-# INLINABLE (*^) #-}

-- | right scalar multiplication
(^*)   :: (Num (IxValue vector), VectorLike_ vector)
       => vector -> IxValue vector -> vector
v ^* s = s *^ v
{-# INLINABLE (^*) #-}

-- | scalar division
(^/)   :: (VectorLike_ vector, Fractional (IxValue vector))
       => vector -> IxValue vector -> vector
v ^/ s = v ^* (1/s)
{-# INLINABLE (^/) #-}

-- | sum a collection of vectors.
sumV :: (Foldable f, Additive_ vector, Num (IxValue vector)) => f vector -> vector
sumV = F.foldl' (^+^) zero
{-# INLINABLE sumV #-}

-- | Produce a default basis for a vector space. If the dimensionality
-- of the vector space is not statically known, see 'basisFor'.
basis :: (Additive_ vector, Num (IxValue vector)) => [vector]
basis = basisFor zero
{-# INLINABLE basis #-}

-- | Produce a default basis for a vector space from which the
-- argument is drawn.
basisFor :: (Additive_ vector, Num (IxValue vector)) => vector -> [vector]
basisFor = \t ->
   ifoldMapOf components ?? t $ \i _ ->
     return                  $
       iover  components ?? t $ \j _ ->
         if i == j then 1 else 0
{-# INLINABLE basisFor #-}

-- instance Monoid c => Additive_ (Const c a) where
--   zero = Const mempty
--   liftU2 _f l _ = l
--   liftI2 _f l _ = l
