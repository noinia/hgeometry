{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Vector.Impl
  ( generate
  , component, xComponent, yComponent, zComponent, wComponent

  , zero, (^+^), (^-^), lerp, negated, (*^), (^*), (^/), liftI2, sumV, basis, unit
  , foldMapZip


  , dot, quadrance, qd, norm, signorm

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
import qualified Data.Vector.Generic as GV
import           Data.Vector.Generic.Mutable (MVector(basicInitialize))
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           GHC.TypeLits (natVal, KnownNat)
import           HGeometry.Vector.Class(VectorLike_(..), Additive_(..))
import           R
import           Vector
import qualified HGeometry.Number.Radical as Radical


--------------------------------------------------------------------------------
-- * Basic Vector Operations

-- | Generate a vector from a given function.
generate   :: (R ~ IxValue (Vector D R))
           => (Int -> R) -> Vector D R
generate f = runIdentity $ generateA (Identity . f)
{-# INLINE generate #-}

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
component :: forall i. (i < D, KnownNat i, R ~ IxValue (Vector D R))
          => IndexedLens' Int (Vector D R) R
component = singular $ component' (fromInteger . natVal $ Proxy @i)
{-# INLINE component #-}

-- | Shorthand for accessing the x-component
xComponent :: (0 < D, R ~ IxValue (Vector D R))
           => IndexedLens' Int (Vector D R) R
xComponent = component @0
{-# INLINE xComponent #-}

-- | Shorthand for accessing the x-component
yComponent :: (1 < D, R ~ IxValue (Vector D R))
           => IndexedLens' Int (Vector D R) R
yComponent = component @1
{-# INLINE yComponent #-}

-- | Shorthand for accessing the x-component
zComponent :: (2 < D, R ~ IxValue (Vector D R))
           => IndexedLens' Int (Vector D R) R
zComponent = component @2
{-# INLINE zComponent #-}

-- | Shorthand for accessing the x-component
wComponent :: (3 < D, R ~ IxValue (Vector D R))
           => IndexedLens' Int (Vector D R) R
wComponent = component @3
{-# INLINE wComponent #-}

--------------------------------------------------------------------------------
-- * Additive Operations

infixl 7 ^*, *^, ^/
infixl 6 ^+^, ^-^


-- | zero vector
zero :: (Num R, R ~ IxValue (Vector D R)) => Vector D R
zero = generate (const 0)
{-# INLINE zero #-}

-- | unit vector
unit :: (Num R, R ~ IxValue (Vector D R)) => Vector D R
unit = over components (const 1) zero
{-# INLINE unit #-}

-- | add two vectors
(^+^)   :: (Num R, R ~ IxValue (Vector D R))
        => Vector D R -> Vector D R -> Vector D R
u ^+^ v = liftU2 (+) u v
{-# INLINE (^+^) #-}

-- | subtract vectors
(^-^)   :: (Num R,  R ~ IxValue (Vector D R))
        => Vector D R -> Vector D R -> Vector D R
u ^-^ v = u ^+^ negated v
{-# INLINE (^-^) #-}

-- | Linearly interpolate between the two vectors
lerp           :: (Num R, R ~ IxValue (Vector D R))
               => R -> Vector D R -> Vector D R -> Vector D R
lerp alpha u v = alpha *^ u ^+^ (1-alpha) *^ v
{-# INLINE lerp #-}

-- | Apply a function to the components of two vectors.
liftI2       :: (R ~ IxValue (Vector D R))
             => (R -> R -> R)
             -> Vector D R -> Vector D R -> Vector D R
liftI2 f u v = runIdentity $ liftI2A (\x x' -> Identity $ f x x') u v
{-# INLINE liftI2 #-}

-- | negate v
negated :: (Num R, R ~ IxValue (Vector D R)) => Vector D R -> Vector D R
negated = ((-1) *^)
{-# INLINABLE negated #-}

-- | left scalar multiplication
(*^)   :: (Num R, R ~ IxValue (Vector D R)) => R -> Vector D R -> Vector D R
s *^ v = over components (s*) v
{-# INLINABLE (*^) #-}

-- | right scalar multiplication
(^*)   :: (Num R, R ~ IxValue (Vector D R))
       => Vector D R -> R -> Vector D R
v ^* s = s *^ v
{-# INLINABLE (^*) #-}

-- | scalar division
(^/)   :: (Fractional R, R ~ IxValue (Vector D R))
       => Vector D R -> R -> Vector D R
v ^/ s = v ^* (1/s)
{-# INLINABLE (^/) #-}

-- | sum a collection of vectors.
sumV :: (Foldable f, Num R, R ~ IxValue (Vector D R)) => f (Vector D R) -> Vector D R
sumV = F.foldl' (^+^) zero
{-# INLINABLE sumV #-}

-- | Produce a default basis for a vector space. If the dimensionality
-- of the vector space is not statically known, see 'basisFor'.
basis :: (Num R, R ~ IxValue (Vector D R)) => [Vector D R]
basis = basisFor zero
{-# INLINABLE basis #-}

-- | Produce a default basis for a vector space from which the
-- argument is drawn.
basisFor :: (Num R, R ~ IxValue (Vector D R)) => Vector D R-> [Vector D R]
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
foldMapZip       :: (Semigroup m, R ~ IxValue (Vector D R))
                 => (R -> R -> m) -> Vector D R -> Vector D R -> m
foldMapZip f u v = getConst $ liftI2A (\x x' -> Const $ f x x') u v
{-# INLINE foldMapZip #-}

--------------------------------------------------------------------------------
-- * Metric

-- | Compute the inner product of two vectors or (equivalently)
-- convert a vector f a into a covector f a -> a.
dot :: (Num R, R ~ IxValue (Vector D R)) => Vector D R -> Vector D R -> R
dot u v = sumOf components $ liftI2 (*) u v
{-# INLINE dot #-}

-- | Compute the squared norm. The name quadrance arises from Norman
-- J. Wildberger's rational trigonometry.
quadrance   :: (Num R, R ~ IxValue (Vector D R)) => Vector D R -> R
quadrance v = dot v v
{-# INLINE quadrance #-}

-- | Compute the quadrance of the difference
qd     :: (Num R, R ~ IxValue (Vector D R)) => Vector D R -> Vector D R -> R
qd u v = quadrance $ u ^-^ v
{-# INLINE qd #-}

-- -- | Compute the distance between two vectors in a metric space
-- distance :: Radical R => vector -> vector -> R

-- | Compute the norm of a vector in a metric space
norm :: (Radical.Radical R, R ~ IxValue (Vector D R)) => Vector D R -> R
norm = Radical.sqrt . quadrance
{-# INLINE norm #-}

-- | Convert a non-zero vector to unit vector.
signorm   :: ( Radical.Radical R
             , Fractional R
             , R ~ IxValue (Vector D R)
             ) => Vector D R -> Vector D R
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
isScalarMultipleOf       :: (Eq R, Fractional R, R ~ IxValue (Vector D R))
                         => Vector D R -> Vector D R  -> Bool
u `isScalarMultipleOf` v = let d' = u `dot` v
                               num = quadrance u * quadrance v
                           in num == 0 || num == d'*d'
{-# INLINE isScalarMultipleOf #-}

-- | Given two colinar vectors, u and v, test if they point in the same direction, i.e.
-- iff scalarMultiple' u v == Just lambda, with lambda > 0
--
-- pre: u and v are colinear, u and v are non-zero
sameDirection     :: ( Num R, Eq R,  R ~ IxValue (Vector D R)
                     ) => Vector D R -> Vector D R -> Bool
sameDirection u v = getAll $ foldMapZip (\ux vx -> All $ signum ux == signum vx) u v
{-# INLINE sameDirection #-}

-- | scalarMultiple u v computes the scalar labmda s.t. v = lambda * u (if it exists)
scalarMultiple     :: (Eq R, Fractional R, R ~ IxValue (Vector D R))
                   => Vector D R -> Vector D R -> Maybe R
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
scalarMultiple'      :: (Eq R, Fractional R, R ~ IxValue (Vector D R))
                     => Vector D R -> Vector D R -> Maybe R
scalarMultiple' u v = g $ foldMapZip f u v
  where
    f 0  0  = Maybe -- we don't know lambda yet, but it may still be a scalar mult.
    f _  0  = No      -- Not a scalar multiple
    f ui vi = Yes $ ui / vi -- can still be a scalar multiple

    g No      = Nothing
    g Maybe   = error "scalarMultiple': found a Maybe, which means the vectors either have length zero, or one of them is all Zero!"
    g (Yes x) = Just x


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * unboxed vectors

-- | elements of the vector are stored consecutively
newtype instance UMV.MVector s (Vector D R) = MV_VectorD (UMV.MVector s R)
newtype instance UV.Vector     (Vector D R) = V_VectorD  (UV.Vector     R)

d :: Int
d = fromInteger $ natVal (Proxy @D)

instance ( GMV.MVector UMV.MVector R
         , VectorLike_ (Vector D R)
         , R ~ IxValue (Vector D R)
         ) => GMV.MVector UMV.MVector (Vector D R) where

  basicLength (MV_VectorD v) = GMV.basicLength v `div` d
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_VectorD v) = MV_VectorD $ GMV.basicUnsafeSlice (d*s) (d*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_VectorD v) (MV_VectorD v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_VectorD <$> GMV.basicUnsafeNew (d*n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_VectorD v) = GMV.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_VectorD v) i = generateA $ \j -> GMV.basicUnsafeRead v (d*i+j)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_VectorD v) i w = imapMOf_ components f w
    where
      f j x = GMV.basicUnsafeWrite v (d*i+j) x
  {-# INLINE basicUnsafeWrite #-}


instance ( GV.Vector UV.Vector R
         , VectorLike_ (Vector D R)
         , R ~ IxValue (Vector D R)
         ) => GV.Vector UV.Vector (Vector D R) where

  basicUnsafeFreeze (MV_VectorD mv) = V_VectorD <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_VectorD v) = MV_VectorD <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_VectorD v) = GV.basicLength v `div` d
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_VectorD v) = V_VectorD $ GV.basicUnsafeSlice (d*s) (d*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_VectorD v) i = generateA $ \j -> GV.basicUnsafeIndexM v (d*i+j)
  {-# INLINE basicUnsafeIndexM #-}

instance ( UV.Unbox R, VectorLike_ (Vector D R), R ~ IxValue (Vector D R)
         ) => UV.Unbox (Vector D R) where
