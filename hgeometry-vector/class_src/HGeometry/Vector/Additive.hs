module HGeometry.Vector.Additive
  ( Additive_(..), negated, (*^), (^*), (^/), sumV, basis, unit
  , HasComponents(..)
  ) where

import           Control.Lens
import qualified Data.Foldable as F

--------------------------------------------------------------------------------

class HasComponents vector vector' where
  -- | An Indexed Traversal over the components of a vector
  components :: IndexedTraversal Int vector vector' (IxValue vector) (IxValue vector')

infixl 6 ^+^, ^-^
infixl 7 ^*, *^, ^/

-- | Basically a copy of the Linear.Additive class
class HasComponents vector vector => Additive_ vector where
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

  {-# MINIMAL zero, liftU2, liftI2 #-}

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
