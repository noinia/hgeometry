--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Additive
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- typeclass that expresses that we can essentially add vectors
--
--------------------------------------------------------------------------------
module HGeometry.Vector.Additive
  ( Additive_(..), negated, (*^), (^*), (^/), sumV, basis, unit
  , HasComponents(..)
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import qualified Linear.V1 as LinearV1
import qualified Linear.V2 as LinearV2
import qualified Linear.V3 as LinearV3
import qualified Linear.V4 as LinearV4
import qualified Linear.Vector as Linear

--------------------------------------------------------------------------------

-- | Types that have a 'components' indexed traversal
class HasComponents vector vector' where
  -- | An Indexed Traversal over the components of a vector
  components :: IndexedTraversal Int vector vector' (IxValue vector) (IxValue vector')

infixl 6 ^+^, ^-^
infixl 7 ^*, *^, ^/

type instance IxValue (Const c a) = a

instance HasComponents (Const c a) (Const c b) where
  components _paFb (Const c) = pure (Const c)
  {-# INLINE components #-}

instance HasComponents (LinearV1.V1 a) (LinearV1.V1 b) where
  components = liftItraverse $ \f (LinearV1.V1 x) -> LinearV1.V1 <$> f 0 x
  {-# INLINE components #-}
instance HasComponents (LinearV2.V2 a) (LinearV2.V2 b) where
  components = liftItraverse $ \f (LinearV2.V2 x y) -> LinearV2.V2 <$> f 0 x <*> f 1 y
  {-# INLINE components #-}
instance HasComponents (LinearV3.V3 a) (LinearV3.V3 b) where
  components = liftItraverse $ \f (LinearV3.V3 x y z) -> LinearV3.V3 <$> f 0 x <*> f 1 y <*> f 2 z
  {-# INLINE components #-}
instance HasComponents (LinearV4.V4 a) (LinearV4.V4 b) where
  components = liftItraverse $
               \f (LinearV4.V4 x y z w) -> LinearV4.V4 <$> f 0 x <*> f 1 y <*> f 2 z <*> f 3 w
  {-# INLINE components #-}

-- | Helper function to use the default traversable instance, and lift a custom itraverse instance
liftItraverse       :: Traversable v
                    => (forall f. Applicative f => (Int -> a -> f b) -> v a -> f (v b))
                    -> IndexedTraversal Int (v a) (v b) a b
liftItraverse itrav = conjoined traverse (itrav . indexed)
{-# INLINE liftItraverse #-}


-- | Basically a copy of the Linear.Additive class
class HasComponents vector vector => Additive_ vector where
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

instance Additive_ (LinearV1.V1 r) where
  zero = Linear.zero
  liftU2 = Linear.liftU2
  liftI2 = Linear.liftI2
instance Additive_ (LinearV2.V2 r) where
  zero = Linear.zero
  liftU2 = Linear.liftU2
  liftI2 = Linear.liftI2
instance Additive_ (LinearV3.V3 r) where
  zero = Linear.zero
  liftU2 = Linear.liftU2
  liftI2 = Linear.liftI2
instance Additive_ (LinearV4.V4 r) where
  zero = Linear.zero
  liftU2 = Linear.liftU2
  liftI2 = Linear.liftI2


-- | unit vector
unit :: forall vector. (Additive_ vector, Num (IxValue vector)) => vector
unit = over components (const 1) (zero @vector)
{-# INLINABLE unit #-}

-- | negate v
negated :: (Num (IxValue vector), HasComponents vector vector) => vector -> vector
negated = ((-1) *^)
{-# INLINABLE negated #-}

-- | left scalar multiplication
(*^)   :: (Num (IxValue vector), HasComponents vector vector) => IxValue vector -> vector -> vector
s *^ v = over components (s*) v
{-# INLINABLE (*^) #-}

-- | right scalar multiplication
(^*)   :: (Num (IxValue vector), HasComponents vector vector)
       => vector -> IxValue vector -> vector
v ^* s = s *^ v
{-# INLINABLE (^*) #-}

-- | scalar division
(^/)   :: (HasComponents vector vector, Fractional (IxValue vector))
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

instance Monoid c => Additive_ (Const c a) where
  zero = Const mempty
  liftU2 _f l _ = l
  liftI2 _f l _ = l
