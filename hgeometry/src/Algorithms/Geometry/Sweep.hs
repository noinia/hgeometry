{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.Sweep
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Helper types and functions for implementing Sweep line algorithms.
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.Sweep where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Proxy
import           Data.Reflection
import           Unsafe.Coerce

--------------------------------------------------------------------------------

newtype Tagged (s :: *) a = Tagged { unTag :: a} deriving (Show,Eq,Ord)

tag   :: proxy s -> a -> Tagged s a
tag _ = Tagged


-- | Represent a computation that needs a particular time as input.
newtype Timed s t a = Timed {atTime :: (Tagged s t) -> a }


instance (Reifies s t, Ord k) => Ord (Timed s t k) where
  compare = compare_

instance (Reifies s t, Ord k) => Eq (Timed s t k) where
  a == b = a `compare` b == EQ

-- | Comparison function for timed values
compare_                       :: forall s t k. (Ord k, Reifies s t)
                               => Timed s t k -> Timed s t k
                               -> Ordering
(Timed f) `compare_` (Timed g) = let t = reflect (Proxy :: Proxy s)
                                 in f (Tagged t) `compare` g (Tagged t)


-- | Coerce timed values
coerceTo   :: proxy s -> f (Timed s' t k) v -> f (Timed s t k) v
coerceTo _ = unsafeCoerce


unTagged :: f (Timed s t k) v -> f (Timed () t k) v
unTagged = coerceTo (Proxy :: Proxy ())


-- | Runs a computation at a given time.
runAt       :: forall s0 t k r f v. Ord k
            => t
            -> f (Timed s0 t k) v
            -> (forall s. Reifies s t => f (Timed s t k) v -> r)
            -> r
runAt t m f = reify t $ \prx -> f (coerceTo prx m)


--------------------------------------------------------------------------------


getTime :: Timed s Int Int
getTime = Timed unTag


constT     :: proxy s -> Int -> Timed s Int Int
constT _ i = Timed (const i)


test1 i = reify 5 $ \prx -> getTime < constT prx i





test2M   :: Reifies s Int => proxy s -> Map (Timed s Int Int) String
test2M p = Map.fromList [ (constT p 10, "ten")
                        , (getTime, "timed")
                        ]


query :: forall s v. Ord (Timed s Int Int)
      => Map (Timed s Int Int) v -> Maybe v
query = fmap snd . Map.lookupGE (constT (Proxy :: Proxy s) 4)


test2   :: Int -> Maybe String
test2 t = runAt t m query
  where
    m :: Map (Timed () Int Int) String
    m = reify 0 $ \p -> unTagged $ test2M p





-- test2 = reify 0 $ \p0 ->
--                     let m = unTagged $ test2M p0
--                     in runAt 10 m Map.lookup



-- newtype Key s a b = Key { getKey :: a -> b }

-- instance (Eq b, Reifies s a) => Eq (Key s a b) where
--   (Key f) == (Key g) = let x = reflect (Proxy :: Proxy s)
--                        in f x == g x

-- instance (Ord b, Reifies s a) => Ord (Key s a b) where
--   Key f `compare` Key g = let x = reflect (Proxy :: Proxy s)
--                           in f x `compare` g x


-- -- | Query the sweep
-- queryAt       :: a
--               -> (forall (s :: *). Reifies s a => Map (Key s a b) v -> res)
--               -> Map (a -> b) v -> res
-- queryAt x f m = reify x (\p -> f . coerceKeys p $ m)

-- updateAt      :: a
--               -> (forall (s :: *). Reifies s a =>
--                    Map (Key s a b) v -> Map (Key s a b) v')
--               -> Map (a -> b) v
--               -> Map (a -> b) v'
-- updateAt x f m = reify x (\p -> uncoerceKeys . f . coerceKeys p $ m)


-- combineAt            :: a
--                      -> (forall (s :: *). Reifies s a =>
--                            Map (Key s a b) v -> Map (Key s a b) v
--                            -> Map (Key s a b) v)
--                      -> Map (a -> b) v
--                      -> Map (a -> b) v
--                      -> Map (a -> b) v
-- combineAt x uF m1 m2 = reify x (\p -> uncoerceKeys $
--                                         coerceKeys p m1 `uF` coerceKeys p m2)


-- splitLookupAt       :: Ord b
--                     => a
--                     -> (a -> b)
--                     -> Map (a -> b) v
--                     -> (Map (a -> b) v, Maybe v, Map (a -> b) v)
-- splitLookupAt x k m = reify x (\p -> let (l,mv,r) = Map.splitLookup (Key k)
--                                                   $ coerceKeys p m
--                                    in (uncoerceKeys l, mv, uncoerceKeys r))


-- --------------------------------------------------------------------------------

-- coerceKeys   :: proxy s -> Map (a -> b) v -> Map (Key s a b) v
-- coerceKeys _ = unsafeCoerce

-- uncoerceKeys :: Map (Key s a b) v -> Map (a -> b) v
-- uncoerceKeys = unsafeCoerce


-- --------------------------------------------------------------------------------


-- data Node a = Node2 a a
--             | Node3 a a a

-- data FT a = Single a
--           | Deep (FT (Node a)) a (FT (Node a))
