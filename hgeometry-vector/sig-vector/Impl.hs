{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Impl
  ( Vector, pattern Vector1_, pattern Vector2_
    -- * Constructing Vectors.
  ,  generate
  -- * Additonal Accessors
  , component, xComponent, yComponent, zComponent, wComponent
  -- * Applicative operations
  , zero, (^+^), (^-^), lerp, negated, (*^), (^*), (^/), liftI2, sumV, basis, unit
  , foldMapZip
  -- * Metric operations
  , dot, quadrance, qd, norm, signorm
  -- * Other
  , sameDirection
  , scalarMultiple
  , isScalarMultipleOf
  ) where

import           Control.Lens
import           D
import qualified Data.Foldable as F
import           Data.Proxy
import           Data.Semigroup
import           Data.Type.Ord
import           GHC.TypeLits (natVal, KnownNat)
import qualified HGeometry.Number.Radical as Radical
import           HGeometry.Vector.Class
import           R
import           Vector

--------------------------------------------------------------------------------
-- * Basic Vector Operations

-- | Generate a vector from a given function.
generate   :: (Int -> R) -> Vector
generate f = runIdentity $ generateA (Identity . f)
{-# INLINE generate #-}

--------------------------------------------------------------------------------

-- | Construct or Destruct a 2 dimensional vector.
pattern Vector1_ :: (0 < D, Vector_ Vector 1 R) => R -> Vector
pattern Vector1_ x <- (v1 -> x)
  where
    Vector1_ x = generate $ const x
{-# INLINE Vector1_ #-}
{-# COMPLETE Vector1_ #-}

v1 :: 0 < D => Vector -> R
v1 = view xComponent
{-# INLINE v1 #-}

----------------------------------------

-- | Construct or Destruct a 2 dimensional vector.
pattern Vector2_ :: (0 < D, 1 < D, Vector_ Vector 2 R) => R -> R -> Vector
pattern Vector2_ x y <- (v2 -> (x,y))
  where
    Vector2_ x y = generate $ \case
                                0 -> x
                                _ -> y
{-# INLINE Vector2_ #-}
{-# COMPLETE Vector2_ #-}

v2   :: (0 < D, 1 < D) => Vector -> (R,R)
v2 v = (v^.xComponent, v^.yComponent)
{-# INLINE v2 #-}

----------------------------------------


--------------------------------------------------------------------------------

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
component :: forall i. (i < D, KnownNat i)
          => IndexedLens' Int Vector R
component = singular $ component' (fromInteger . natVal $ Proxy @i)
{-# INLINE component #-}

-- | Shorthand for accessing the x-component
xComponent :: (0 < D)
           => IndexedLens' Int Vector R
xComponent = component @0
{-# INLINE xComponent #-}

-- | Shorthand for accessing the x-component
yComponent :: (1 < D)
           => IndexedLens' Int Vector R
yComponent = component @1
{-# INLINE yComponent #-}

-- | Shorthand for accessing the x-component
zComponent :: (2 < D)
           => IndexedLens' Int Vector R
zComponent = component @2
{-# INLINE zComponent #-}

-- | Shorthand for accessing the x-component
wComponent :: (3 < D)
           => IndexedLens' Int Vector R
wComponent = component @3
{-# INLINE wComponent #-}

--------------------------------------------------------------------------------
-- * Additive Operations

infixl 7 ^*, *^, ^/
infixl 6 ^+^, ^-^

-- | zero vector
zero :: (Num R) => Vector
zero = generate (const 0)
{-# INLINE zero #-}

-- | unit vector
unit :: (Num R) => Vector
unit = over components (const 1) zero
{-# INLINE unit #-}

-- | add two vectors
(^+^)   :: (Num R)
        => Vector -> Vector -> Vector
u ^+^ v = liftU2 (+) u v
{-# INLINE (^+^) #-}

-- | subtract vectors
(^-^)   :: (Num R)
        => Vector -> Vector -> Vector
u ^-^ v = u ^+^ negated v
{-# INLINE (^-^) #-}

-- | Linearly interpolate between the two vectors
lerp           :: (Num R)
               => R -> Vector -> Vector -> Vector
lerp alpha u v = alpha *^ u ^+^ (1-alpha) *^ v
{-# INLINE lerp #-}

-- | Apply a function to the components of two vectors.
liftI2       :: (R -> R -> R)
             -> Vector -> Vector -> Vector
liftI2 f u v = runIdentity $ liftI2A (\x x' -> Identity $ f x x') u v
{-# INLINE liftI2 #-}

-- | negate v
negated :: (Num R) => Vector -> Vector
negated = ((-1) *^)
{-# INLINABLE negated #-}

-- | left scalar multiplication
(*^)   :: (Num R) => R -> Vector -> Vector
s *^ v = over components (s*) v
{-# INLINABLE (*^) #-}

-- | right scalar multiplication
(^*)   :: (Num R)
       => Vector -> R -> Vector
v ^* s = s *^ v
{-# INLINABLE (^*) #-}

-- | scalar division
(^/)   :: (Fractional R)
       => Vector -> R -> Vector
v ^/ s = v ^* (1/s)
{-# INLINABLE (^/) #-}

-- | sum a collection of vectors.
sumV :: (Foldable f, Num R) => f Vector -> Vector
sumV = F.foldl' (^+^) zero
{-# INLINABLE sumV #-}

-- | Produce a default basis for a vector space. If the dimensionality
-- of the vector space is not statically known, see 'basisFor'.
basis :: (Num R) => [Vector]
basis = basisFor zero
{-# INLINABLE basis #-}

-- | Produce a default basis for a vector space from which the
-- argument is drawn.
basisFor :: (Num R) => Vector-> [Vector]
basisFor = \t ->
   ifoldMapOf components ?? t $ \i _ ->
     return                  $
       iover  components ?? t $ \j _ ->
         if i == j then 1 else 0
{-# INLINABLE basisFor #-}

-- | "zip through the two vectors", folding over the result.
--
-- as an example, we can implement the dot product of two vectors u and v using:
--
-- >>> let myDot u v = getSum $ foldMapZip (\x x' -> Sum $ x * x') u v
-- >>> myDot (Vector3 1 2 3) (Vector3 10 20 30)
-- 140
foldMapZip       :: (Semigroup m)
                 => (R -> R -> m) -> Vector -> Vector -> m
foldMapZip f u v = getConst $ liftI2A (\x x' -> Const $ f x x') u v
{-# INLINE foldMapZip #-}

--------------------------------------------------------------------------------
-- * Metric

-- | Compute the inner product of two vectors or (equivalently)
-- convert a vector f a into a covector f a -> a.
dot :: (Num R) => Vector -> Vector -> R
dot u v = sumOf components $ liftI2 (*) u v
{-# INLINE dot #-}

-- | Compute the squared norm. The name quadrance arises from Norman
-- J. Wildberger's rational trigonometry.
quadrance   :: (Num R) => Vector -> R
quadrance v = dot v v
{-# INLINE quadrance #-}

-- | Compute the quadrance of the difference
qd     :: (Num R) => Vector -> Vector -> R
qd u v = quadrance $ u ^-^ v
{-# INLINE qd #-}

-- -- | Compute the distance between two vectors in a metric space
-- distance :: Radical R => vector -> vector -> R

-- | Compute the norm of a vector in a metric space
norm :: (Radical.Radical R) => Vector -> R
norm = Radical.sqrt . quadrance
{-# INLINE norm #-}

-- | Convert a non-zero vector to unit vector.
signorm   :: ( Radical.Radical R
             , Fractional R
             ) => Vector -> Vector
signorm v = v ^/ norm v
{-# INLINE signorm #-}

--------------------------------------------------------------------------------

-- | 'isScalarmultipleof u v' test if v is a scalar multiple of u.
--
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 10 10
-- True
-- >>> Vector3 1 1 2 `isScalarMultipleOf` Vector3 10 10 20
-- True
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 10 1
-- False
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 (-1) (-1)
-- True
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 11.1 11.1
-- True
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 11.1 11.2
-- False
-- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 11.1 11.2
-- False
-- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 4 2
-- True
-- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 4 0
-- False
-- >>> Vector3 2 1 0 `isScalarMultipleOf` Vector3 4 0 5
-- False
-- >>> Vector3 0 0 0 `isScalarMultipleOf` Vector3 4 0 5
-- True
isScalarMultipleOf       :: (Eq R, Fractional R)
                         => Vector -> Vector  -> Bool
u `isScalarMultipleOf` v = let d' = u `dot` v
                               num = quadrance u * quadrance v
                           in num == 0 || num == d'*d'
{-# INLINE isScalarMultipleOf #-}

-- | Given two colinar vectors, u and v, test if they point in the same direction, i.e.
-- iff scalarMultiple' u v == Just lambda, with lambda > 0
--
-- pre: u and v are colinear, u and v are non-zero
sameDirection     :: (Num R, Eq R) => Vector -> Vector -> Bool
sameDirection u v = getAll $ foldMapZip (\ux vx -> All $ signum ux == signum vx) u v
{-# INLINE sameDirection #-}

-- | scalarMultiple u v computes the scalar labmda s.t. v = lambda * u (if it exists)
scalarMultiple     :: (Eq R, Fractional R)
                   => Vector -> Vector -> Maybe R
scalarMultiple u v
      | allZero u || allZero v = Just 0
      | otherwise              = scalarMultiple' u v
  where
    allZero = allOf components (== 0)
{-# INLINABLE scalarMultiple #-}

data ScalarMultiple r = No | Maybe | Yes r deriving (Eq,Show)

instance Eq r => Semigroup (ScalarMultiple r) where
  No      <> _       = No
  _       <> No      = No
  Maybe   <> x       = x
  x       <> Maybe   = x
  (Yes x) <> (Yes y)
     | x == y               = Yes x
     | otherwise            = No

instance Eq r => Monoid (ScalarMultiple r) where
  mempty = Maybe
  mappend = (<>)

-- | Actual implementation of scalarMultiple
scalarMultiple'      :: (Eq R, Fractional R)
                     => Vector -> Vector -> Maybe R
scalarMultiple' u v = g $ foldMapZip f u v
  where
    f 0  0  = Maybe -- we don't know lambda yet, but it may still be a scalar mult.
    f _  0  = No      -- Not a scalar multiple
    f ui vi = Yes $ ui / vi -- can still be a scalar multiple

    g No      = Nothing
    g Maybe   = error "scalarMultiple': found a Maybe, which means the vectors either have length zero, or one of them is all Zero!"
    g (Yes x) = Just x


--------------------------------------------------------------------------------
