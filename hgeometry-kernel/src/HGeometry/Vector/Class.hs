{-# LANGUAGE AllowAmbiguousTypes #-}
module HGeometry.Vector.Class
  ( Vector_(..), pattern Vector1_, pattern Vector2_, pattern Vector3_, pattern Vector4_
  , element

  , prefix
  , Additive_(..), negated, (*^), (^*), (^/), sumV, basis, unit
  , HasElements(..)
  ) where

import           Control.Arrow ((&&&))
import           Control.Lens hiding (element, elements)
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Type.Ord
import           GHC.TypeNats
import           HGeometry.Properties
import           HGeometry.Vector.Additive

--------------------------------------------------------------------------------


-- | A type class for vectors
class ( NumType vector   ~ r
      , Dimension vector ~ d
      , IxValue vector ~ r
      , Index vector ~ Int
      , Ixed vector
      , HasElements vector vector
      ) => Vector_ vector d r | vector -> d
                              , vector -> r where

  -- | Access the i^th element.
  elementProxy   :: forall i proxy. (i < d, KnownNat i) => proxy i -> IndexedLens' Int vector r
  elementProxy i = iix' (fromIntegral . natVal $ i)
    where
      iix'   :: Ixed s => Index s -> IndexedLens' (Index s) s (IxValue s)
      iix' j = singular $ iix j
  {-# INLINE elementProxy #-}

  -- | try to construct a vector from a list of exactly d coordinates.
  vectorFromList :: [r] -> Maybe vector

  {-# MINIMAL vectorFromList #-}


-- | A bidirectional pattern synonym for 1 dimensional vectors.
pattern Vector1_   :: Vector_ vector 1 r => r -> vector
pattern Vector1_ x <- (view (element @0) -> x)
  where
    Vector1_ x = uncheckedVectorFromList [x]

-- | A bidirectional pattern synonym for 2 dimensional vectors.
pattern Vector2_     :: Vector_ vector 2 r => r -> r -> vector
pattern Vector2_ x y <- (view (element @0) &&& view (element @1) -> (x,y))
  where
    Vector2_ x y = uncheckedVectorFromList [x,y]


-- | A bidirectional pattern synonym for 3 dimensional vectors.
pattern Vector3_       :: Vector_ vector 3 r => r -> r -> r -> vector
pattern Vector3_ x y z <- (view (element @0) &&& view (element @1) &&& view (element @2)
                          -> (x,(y,z)))
  where
    Vector3_ x y z = uncheckedVectorFromList [x,y,z]

-- | A bidirectional pattern synonym for 4 dimensional vectors.
pattern Vector4_         :: Vector_ vector 4 r => r -> r -> r -> r -> vector
pattern Vector4_ x y z w <- (   (view (element @0) &&& view (element @1)
                             &&& view (element @2) &&& view (element @3))
                            -> (x,(y,(z,w))))
  where
    Vector4_ x y z w = uncheckedVectorFromList [x,y,z,w]




-- | Lens to access te i^t element.
element :: forall i vector d r. (Vector_ vector d r, i < d, KnownNat i)
        => IndexedLens' Int vector r
element = elementProxy (Proxy @i)
{-# INLINE element #-}

-- | Construct a vector from a list of exactly d elements. Crashes
-- when we get the wrong number of elements.
uncheckedVectorFromList :: Vector_ vector d r => [r] -> vector
uncheckedVectorFromList = fromMaybe (error "uncheckedVectorFromList") . vectorFromList
{-# INLINABLE uncheckedVectorFromList #-}

--------------------------------------------------------------------------------

-- | Take a prefix of length i of the vector
prefix :: forall i d vector vector' r. ( i <= d, KnownNat i
                                       , Vector_ vector d r, Vector_ vector' i r)
       => vector -> vector'
prefix = uncheckedVectorFromList . List.genericTake (natVal $ Proxy @i) . toListOf elements

-- cons     :: (Vector_ vector d r, Vector_ vector' (d+1) r)
--          => r -> vector -> vector'
-- cons x v = uncheckedVectorFromList $ x : (v^..elements)



--------------------------------------------------------------------------------


-- class Vector vec where
--   -- | construct from list
--   fromList :: [r] -> vec r
--   -- | access ith element
--   element :: Int -> vec r -> r

--   pattern V2 :: r -> r -> V2 r
--   -- default implementation:
--   pattern (V2 x y) <- (elment 1 &&& element 2 -> (x,y))
--     where
--       V2 x y = fromList [x,y]

-- data MyV2 r = MyV2 r r

-- instance Vector MyV2 where
--   fromList = undefined
--   element = undefined
--   -- for whatever reason I may want some specialized implementation here.
--   pattern V2 x y = MyV2 x y
