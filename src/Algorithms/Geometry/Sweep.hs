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

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Reflection
import           Unsafe.Coerce

--------------------------------------------------------------------------------

-- $setup
-- >>> type Time = Int
--

-- | Represent a computation that needs a particular time t as input, and
-- produces an a.
newtype Timed (s :: *) t a = Timed (t -> a)
                           deriving (Functor)

instance Applicative (Timed s t) where
  pure = Timed . const
  (Timed f) <*> (Timed x) = Timed $ \t -> (f t) (x t)

instance Monad (Timed s t) where
  return = pure
  -- Timed s t a -> (a -> Timed s t b)
  (Timed m) >>= k = Timed $ \t -> let Timed f = k $ m t
                                  in f t

instance (Reifies s t, Show t, Show a) => Show (Timed s t a) where
  show (Timed f) = let t = reflect (Proxy :: Proxy s)
                   in mconcat ["Timed @", show t, " ", show $ f t]


runAtT             :: t -> Timed s t a -> a
runAtT t (Timed f) = f t

runAtI :: forall s t a. Reifies s t => Timed s t a -> a
runAtI = runAtIP (Proxy :: Proxy s)

runAtIP               :: forall s t a. Reifies s t => Proxy s -> Timed s t a -> a
runAtIP prx (Timed f) = f $ reflect prx

runAt     :: t -> (forall s. Reifies s t => Timed s t a) -> a
runAt t k = reify t $ \prx -> runAtIP prx k


instance (Reifies s t, Ord k) => Ord (Timed s t k) where
  compare = compare_

instance (Reifies s t, Ord k) => Eq (Timed s t k) where
  a == b = a `compare` b == EQ

-- | Comparison function for timed values
compare_                       :: forall s t k. (Ord k, Reifies s t)
                               => Timed s t k -> Timed s t k
                               -> Ordering
(Timed f) `compare_` (Timed g) = let t = reflect (Proxy :: Proxy s)
                                 in f t `compare` g t


-- | A computation that returns the current time.
currentTime :: Timed s t t
currentTime = Timed id

-- | A timed value that always returns the same value (irrespective of time)
constT   :: forall proxy (s :: *) t a. proxy s -> a -> Timed s t a
constT _ = Timed . const





--------------------------------------------------------------------------------

newtype MyLine t = L (t -> Double, String)

instance Show (MyLine t) where
  show (L (_,s)) = s

line1 :: Integral t => MyLine t
line1 = L $ (\x -> fromIntegral x, "Line 1")

line2 ::  Integral t => MyLine t
line2 = L $ (\x -> 10 - fromIntegral x, "Line 2")

--------------------------------------------------------------------------------

pure2 :: f (Timed s t k) v -> Timed s t (f (Timed s t k) v)
pure2 = pure

pure' :: a -> Timed s t (Timed s t a)
pure' = pure . pure


myMap :: (Num t, Ord t, Reifies s0 t) => Map (Timed s0 t t) String
myMap = Map.fromList [ (pure 5, "five")
                     , (currentTime, "timed")
                     ]


myLines :: (Integral t, Ord t, Reifies s0 t) => Map (Timed s0 t Double) (MyLine t)
myLines = Map.fromList $ map f [line1, line2]
  where
    f l@(L (ll,_)) = (ll <$> currentTime, l)


-- | queries at the current time
query'     :: (Ord k, Reifies s t) => k -> Map (Timed s t k) v
           -> Timed s t (Maybe (Timed s t k, v))
query' y m = Map.lookupGE (pure y) <$> pure2 m

-- | queries at the current time
query     :: (Ord k, Reifies s t) => k -> Map (Timed s t k) v -> Timed s t (Maybe v)
query y m = fmap snd <$> query' y m


coerceT :: Map (Timed s0 t k) v -> Map (Timed s t k) v
coerceT = unsafeCoerce


query2       :: (Num t, Ord t, Reifies s0 t) => (t,t) -> Timed s0 t (Maybe String)
query2 (x,y) = f <$> pure2 myMap
  where
    f m = runAt x $ query y (coerceT m)


queryL      :: (Reifies s t, Show t, Show k, Show v, Ord k)
            => k -> Map (Timed s t k) v -> Timed s t (Maybe String)
queryL y m = (fmap show) <$> query' y m

queryL'      :: (Reifies s t, Show t, Show k, Show v, Ord k)
            => k -> Map (Timed s0 t k) v -> Timed s t (Maybe String)
queryL' y m = queryL y (coerceT m)



queryL2       :: (Integral t, Ord t, Show t, Reifies s0 t) => (t,Double) -> Timed s0 t (Maybe String)
queryL2 (x,y) = f <$> pure2 myLines
  where
    -- f   :: Map (Timed s0 t k) String ->
    f m = runAt x $ queryL' y m


-- query2   ::  forall t s s0. (Num t, Ord t, Reifies s t, Reifies s0 t)
--          => t -> Timed s t (Maybe String)
-- query2 y = undefined
--   where
--     x :: Timed s t t
--     x = currentTime

--     lm :: Timed s t (Map (Timed s0 t t) String)
--     lm = pure m

--     m ::(Num t, Ord t, Reifies s0 t) => Map (Timed s0 t t) String
--     m = myMap

-- query2 :: (Ord k, Reifies s0 t, Reifies s t) => (t,t)
--         -> Map (Timed s0 t k) v ->

--------------------------------------------------------------------------------


-- >>> runat 10 $ query 6 myMap
-- Just "timed"
-- >>> runAt 1 $ query 6 myMap
-- Nothing
-- >>> runAt 1 $ query 2 myMap
-- Just "five"
-- >>> runAt 10 $ query 2 myMap
-- Just "five"
test = runAt 1 $ query 2 myMap


-- >>> runAt 0 $ show <$> pure2 myLines
-- "fromList [(Timed @0 0.0,Line 1),(Timed @0 10.0,Line 2)]"
-- >>> runAt 0 $ queryL2 (0,8)
-- Just "(Timed @0 10.0,Line 2)"
-- >>> runAt 0 $ queryL2 (4,8)
-- Nothing
