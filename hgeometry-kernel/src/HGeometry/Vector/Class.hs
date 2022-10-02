{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
module HGeometry.Vector.Class
  ( Vector_(..), pattern Vector1_, pattern Vector2_, pattern Vector3_, pattern Vector4_
  , component
  , xComponent, yComponent, zComponent, wComponent
  , HasComponents(..)

  -- , ConstructVector

  , vectorFromVector
  , prefix, suffix
  , cons, snoc
  , uncons, unsnoc
  , vZipWith

  , Additive_(..), negated, (*^), (^*), (^/), sumV, basis, unit
  , Metric_(..)
  , VectorFor
  ) where

import           Control.Arrow ((&&&))
import           Control.Lens hiding (cons,snoc,uncons,unsnoc)
import           Data.Kind
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Type.Ord
import           GHC.TypeNats
import           HGeometry.Properties
import           HGeometry.Vector.Additive
import           HGeometry.Vector.Metric
import           Prelude hiding (zipWith)

--------------------------------------------------------------------------------

{- $setup
>>> import HGeometry.Vector
-}

--------------------------------------------------------------------------------

-- type ConstructVector :: Type -> Nat -> Type
-- type family ConstructVector vector d where
--   ConstructVector vector 0 = vector
--   ConstructVector vector d = NumType vector -> ConstructVector vector (d-1)

-- | A type class for vectors
type Vector_ :: Type -> Nat -> Type -> Constraint
class ( NumType vector   ~ r
      , Dimension vector ~ d
      , IxValue vector ~ r
      , Index vector ~ Int
      , Ixed vector
      , HasComponents vector vector
      ) => Vector_ vector d r | vector -> d
                              , vector -> r where

  -- | Access the i^th component.
  componentProxy   :: forall i proxy. (i < d, KnownNat i) => proxy i -> IndexedLens' Int vector r
  componentProxy i = iix' (fromIntegral . natVal $ i)
    where
      iix'   :: Ixed s => Index s -> IndexedLens' (Index s) s (IxValue s)
      iix' j = singular $ iix j
  {-# INLINE componentProxy #-}

  -- -- | Construct a vector from a d-arity function
  -- mkVector :: KnownNat d => ConstructVector vector d

  -- | try to construct a vector from a list of exactly d coordinates.
  vectorFromList :: [r] -> Maybe vector

  {-# MINIMAL vectorFromList #-}

  -- | Convert an arbitrary vector into another vector
vectorFromVector :: forall vector vector' d r. (Vector_ vector d r, Vector_ vector' d r)
                 => vector -> vector'
vectorFromVector = uncheckedVectorFromList . toListOf components
{-# INLINE[1] vectorFromVector #-}
{-# RULES
  "vectorFromVector/sameType"
      forall vector. forall (v :: vector). vectorFromVector @vector @vector v = v
  #-}


-- | Zip two vectors together using the given function.
--
vZipWith         :: forall vector vector' vector'' d a b c.
                   (Vector_ vector d a, Vector_ vector' d b, Vector_ vector'' d c)
                => (a -> b -> c) -> vector -> vector' -> vector''
vZipWith f va vb = uncheckedVectorFromList $ List.zipWith f (va^..components) (vb^..components)

--------------------------------------------------------------------------------


-- | A bidirectional pattern synonym for 1 dimensional vectors.
pattern Vector1_   :: Vector_ vector 1 r => r -> vector
pattern Vector1_ x <- (view (component @0) -> x)
  where
    Vector1_ x = uncheckedVectorFromList [x]
{-# COMPLETE Vector1_ #-}

-- | A bidirectional pattern synonym for 2 dimensional vectors.
pattern Vector2_     :: Vector_ vector 2 r => r -> r -> vector
pattern Vector2_ x y <- (view (component @0) &&& view (component @1) -> (x,y))
  where
    Vector2_ x y = uncheckedVectorFromList [x,y]
{-# COMPLETE Vector2_ #-}


-- | A bidirectional pattern synonym for 3 dimensional vectors.
pattern Vector3_       :: Vector_ vector 3 r => r -> r -> r -> vector
pattern Vector3_ x y z <- (view (component @0) &&& view (component @1) &&& view (component @2)
                          -> (x,(y,z)))
  where
    Vector3_ x y z = uncheckedVectorFromList [x,y,z]
{-# COMPLETE Vector3_ #-}

-- | A bidirectional pattern synonym for 4 dimensional vectors.
pattern Vector4_         :: Vector_ vector 4 r => r -> r -> r -> r -> vector
pattern Vector4_ x y z w <- (    view (component @0) &&& view (component @1)
                             &&& view (component @2) &&& view (component @3)
                            -> (x,(y,(z,w))))
  where
    Vector4_ x y z w = uncheckedVectorFromList [x,y,z,w]
{-# COMPLETE Vector4_ #-}


-- | Lens to access te i^t component.
component :: forall i vector d r. (Vector_ vector d r, i < d, KnownNat i)
        => IndexedLens' Int vector r
component = componentProxy (Proxy @i)
{-# INLINE component #-}

-- | Construct a vector from a list of exactly d components. Crashes
-- when we get the wrong number of components.
uncheckedVectorFromList :: Vector_ vector d r => [r] -> vector
uncheckedVectorFromList = fromMaybe (error "uncheckedVectorFromList") . vectorFromList
{-# INLINABLE uncheckedVectorFromList #-}

--------------------------------------------------------------------------------

-- | Take a prefix of length i of the vector
prefix :: forall i d vector vector' r. ( i <= d, KnownNat i
                                       , Vector_ vector d r, Vector_ vector' i r)
       => vector -> vector'
prefix = uncheckedVectorFromList . List.genericTake (natVal $ Proxy @i) . toListOf components

-- | Take a suffix of length i  of the vector
suffix :: forall i d vector vector' r. ( i <= d, KnownNat i, KnownNat d
                                       , Vector_ vector d r, Vector_ vector' i r)
       => vector -> vector'
suffix = uncheckedVectorFromList . List.genericDrop (natVal $ Proxy @(d-i)) . toListOf components


-- | Add an element to the front of the vector
cons     :: (Vector_ vector d r, Vector_ vector' (d+1) r)
         => r -> vector -> vector'
cons x v = uncheckedVectorFromList $ x : (v^..components)

-- | Add an element to the back of the vector.
snoc     :: (Vector_ vector d r, Vector_ vector' (d+1) r)
         => vector -> r -> vector'
snoc v x = uncheckedVectorFromList $ (v^..components) <> [x]

--------------------------------------------------------------------------------

-- | Extract the first element from the vector
uncons   :: forall vector vector' d r.
            ( Vector_ vector (d+1) r, Vector_ vector' d r
            , 0 < d+1 -- this one is silly
            , KnownNat d
            ) => vector -> (r, vector')
uncons v = ( v^.component @0, suffix v)

-- | Extract the last element from the vector
unsnoc   :: forall vector vector' d r.
            (Vector_ vector (d+1) r, Vector_ vector' d r
            , d < d+1 -- these are silly
            , KnownNat d
            ) => vector -> (vector',r)
unsnoc v = ( prefix v, v^.component @d )

--------------------------------------------------------------------------------


-- class Vector vec where
--   -- | construct from list
--   fromList :: [r] -> vec r
--   -- | access ith component
--   component :: Int -> vec r -> r

--   pattern V2 :: r -> r -> V2 r
--   -- default implementation:
--   pattern (V2 x y) <- (elment 1 &&& component 2 -> (x,y))
--     where
--       V2 x y = fromList [x,y]

-- data MyV2 r = MyV2 r r

-- instance Vector MyV2 where
--   fromList = undefined
--   component = undefined
--   -- for whatever reason I may want some specialized implementation here.
--   pattern V2 x y = MyV2 x y



--------------------------------------------------------------------------------
-- * Helper functions specific to two and three dimensional vectors

-- | Shorthand to access the first component
--
-- >>> Vector3 1 2 3 ^. xComponent
-- 1
-- >>> Vector2 1 2 & xComponent .~ 10
-- Vector2 10 2
xComponent :: (0 < d, Vector_ vector d r) => IndexedLens' Int vector r
xComponent = component @0
{-# INLINABLE xComponent #-}

-- | Shorthand to access the second component
--
-- >>> Vector3 1 2 3 ^. yComponent
-- 2
-- >>> Vector2 1 2 & yComponent .~ 10
-- Vector2 1 10
yComponent :: (1 < d, Vector_ vector d r) => IndexedLens' Int vector r
yComponent = component @1
{-# INLINABLE yComponent #-}

-- | Shorthand to access the third component
--
-- >>> Vector3 1 2 3 ^. zComponent
-- 3
-- >>> Vector3 1 2 3 & zComponent .~ 10
-- Vector3 1 2 10
zComponent :: (2 < d, Vector_ vector d r) => IndexedLens' Int vector r
zComponent = component @2
{-# INLINABLE zComponent #-}

-- | Shorthand to access the forth component
--
-- >>> Vector4 1 2 3 4 ^. wComponent
-- 4
-- >>> Vector4 1 2 3 4 & wComponent .~ 10
-- Vector4 1 2 3 10
wComponent :: (3 < d, Vector_ vector d r) => IndexedLens' Int vector r
wComponent = component @3
{-# INLINABLE wComponent #-}

--------------------------------------------------------------------------------

-- | Defines the vector type corresponding to a particular point
type family VectorFor point
