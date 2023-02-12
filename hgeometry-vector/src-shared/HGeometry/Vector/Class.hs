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
{-# LANGUAGE DefaultSignatures #-}
module HGeometry.Vector.Class
  ( Vector
  , VectorLike_(..)
  , component, xComponent, yComponent, zComponent, wComponent
  , Additive_(..), negated, (*^), (^*), (^/), sumV, basis, unit
  , Metric_(..)
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

type instance Index     (Vector d r) = Int
type instance IxValue   (Vector d r) = r
type instance Dimension (Vector d r) = d

--------------------------------------------------------------------------------

-- | Types that have a 'components' indexed traversal
class VectorLike_ vector where
  -- | An Indexed Traversal over the components of a vector
  components :: IndexedTraversal1' Int vector (IxValue vector)
  -- | Access the i^th component. Consider using 'component' instead.
  unsafeComponent :: Int -> IndexedLens' Int vector (IxValue vector)
  default unsafeComponent :: (Index vector ~ Int, Ixed vector)
                          => Int -> IndexedLens' Int vector (IxValue vector)
  unsafeComponent i = singular $ iix i

--------------------------------------------------------------------------------
-- * Generic functions on VectorLike things

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

-- | Shorthand for accessing the x-component
xComponent :: (VectorLike_ vector, 0 < Dimension vector)
           => IndexedLens' Int vector (IxValue vector)
xComponent = component @0
{-# INLINE xComponent #-}

-- | Shorthand for accessing the x-component
yComponent :: (VectorLike_ vector, 1 < Dimension vector)
           => IndexedLens' Int vector (IxValue vector)
yComponent = component @1
{-# INLINE yComponent #-}

-- | Shorthand for accessing the x-component
zComponent :: (VectorLike_ vector, 2 < Dimension vector)
           => IndexedLens' Int vector (IxValue vector)
zComponent = component @2
{-# INLINE zComponent #-}

-- | Shorthand for accessing the x-component
wComponent :: (VectorLike_ vector, 3 < Dimension vector)
           => IndexedLens' Int vector (IxValue vector)
wComponent = component @3
{-# INLINE wComponent #-}


--------------------------------------------------------------------------------
-- * Generic instances

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

instance Additive_ (Vector d r) => Metric_ (Vector d r)

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

--------------------------------------------------------------------------------
-- * Metric

-- | The equivalent class of Linear.Metric
--
-- Note that we do not define a distance itself, and that norm and
-- signorm have a Radical constraint rather than Floating.
class Additive_ vector => Metric_ vector where
  {-# MINIMAL #-}

  -- | Compute the inner product of two vectors or (equivalently)
  -- convert a vector f a into a covector f a -> a.
  dot :: Num (IxValue vector) => vector -> vector -> IxValue vector
  dot u v = sumOf components $ liftI2 (*) u v
  {-# INLINE dot #-}

  -- | Compute the squared norm. The name quadrance arises from Norman
  -- J. Wildberger's rational trigonometry.
  quadrance   :: Num (IxValue vector) => vector -> IxValue vector
  quadrance v = dot v v
  {-# INLINE quadrance #-}

  -- | Compute the quadrance of the difference
  qd     :: Num (IxValue vector) => vector -> vector -> IxValue vector
  qd u v = quadrance $ u ^-^ v
  {-# INLINE qd #-}

  -- -- | Compute the distance between two vectors in a metric space
  -- distance :: Radical (IxValue vector) => vector -> vector -> IxValue vector

--   -- | Compute the norm of a vector in a metric space
--   norm :: Radical (IxValue vector) => vector -> IxValue vector
--   norm = sqrt . quadrance
--   {-# INLINE norm #-}
--
--   -- | Convert a non-zero vector to unit vector.
--   signorm   :: (Radical (IxValue vector), Fractional (IxValue vector)) => vector -> vector
--   signorm v = v ^/ norm v
--   {-# INLINE signorm #-}
