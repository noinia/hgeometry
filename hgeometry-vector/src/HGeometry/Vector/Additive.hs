module HGeometry.Vector.Additive
  ( zero
  , liftU2
  , liftI2
  , (^+^)
  , (^-^)
  , unit, negated
  , (*^)
  , (^*)
  , sumV
  , basis
  , basisFor
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           HGeometry.Sigs.Vector
import           HGeometry.Sigs.Vector.Additive
import           HGeometry.Sigs.R

--------------------------------------------------------------------------------

infixl 6 ^+^, ^-^
infixl 7 ^*, *^

-- | add two vectors
(^+^)   :: Vector -> Vector -> Vector
u ^+^ v = liftU2 (+) u v
{-# INLINE (^+^) #-}

-- | subtract vectors
(^-^)   :: Vector -> Vector -> Vector
u ^-^ v = u ^+^ negated v
{-# INLINE (^-^) #-}

-- | unit vector
unit :: Vector
unit = over components (const 1) zero
{-# INLINE unit #-}

-- | negate v
negated :: Vector -> Vector
negated = ((-1) *^)
{-# INLINE negated #-}

-- | left scalar multiplication
(*^)   :: R -> Vector -> Vector
s *^ v = over components (s*) v
{-# INLINE (*^) #-}

-- | right scalar multiplication
(^*)   :: Vector -> R -> Vector
v ^* s = s *^ v
{-# INLINE (^*) #-}

--------------------------------------------------------------------------------
--

-- | sum a collection of vectors.
sumV :: Foldable f => f Vector -> Vector
sumV = F.foldl' (^+^) zero
{-# INLINE sumV #-}

-- | Produce a default basis for a vector space. If the dimensionality
-- of the vector space is not statically known, see 'basisFor'.
basis :: [Vector]
basis = basisFor zero
{-# INLINE basis #-}

-- | Produce a default basis for a vector space from which the
-- argument is drawn.
basisFor :: Vector -> [Vector]
basisFor = \t ->
   ifoldMapOf components ?? t $ \i _ ->
     return                  $
       iover  components ?? t $ \j _ ->
         if i == j then 1 else 0
{-# INLINE basisFor #-}
