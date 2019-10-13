{-# LANGUAGE UndecidableInstances #-}
module Data.DynamicOrd where

import Data.Proxy
import Data.Reflection
import Unsafe.Coerce

--------------------------------------------------------------------------------

-- Implementation from
-- https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection

-- | Values of type 'a' in our dynamically constructed 'Ord' instance
newtype O (s :: *) (a :: *) = O { runO :: a } deriving (Show)

-- | An Ord Dictionary
newtype OrdDict a = OrdDict { compare_ :: a -> a -> Ordering }

instance Reifies s (OrdDict a) => Eq (O s a) where
  (O l) == (O r) = let cmp = compare_ $ reflect (Proxy :: Proxy s)
                           in case l `cmp` r of
                                EQ -> True
                                _  -> False

instance (Eq (O s a), Reifies s (OrdDict a)) => Ord (O s a) where
  (O l) `compare` (O r) = let cmp = compare_ $ reflect (Proxy :: Proxy s)
                                  in l `cmp` r

-- | Run a computation with a given ordering
withOrd       :: (a -> a -> Ordering) -> (forall s. Reifies s (OrdDict a) => O s b) -> b
withOrd cmp v = reify (OrdDict cmp) (runO . asProxyOf v)
  where
    asProxyOf      :: f s a -> Proxy s -> f s a
    asProxyOf v' _ = v'

--------------------------------------------------------------------------------
-- * Introducing and removing the dynamic order type
-- Note that all of these may be unsafe if used incorrectly

-- | Lifts a container f whose values (of type a) depend on 's' into a
-- more general computation in that produces a 'f a' (depending on s).
--
-- running time: \(O(1)\)
extractOrd1 :: f (O s a) -> O s (f a)
extractOrd1 = unsafeCoerce


-- | Introduce dynamic order in a container 'f'.
--
-- running time: \(O(1)\)
introOrd1 :: f a -> f (O s a)
introOrd1 = unsafeCoerce

-- | Lifts a function that works on a container 'f' of
-- orderable-things into one that works on dynamically ordered ones.
liftOrd1   :: (f (O s a) -> f (O s a))
           -> f a -> O s (f a)
liftOrd1 f = extractOrd1 . f . introOrd1


-- | Lifts a container f whose keys (of type k) depend on 's' into a
-- more general computation in that produces a 'f k v' (depending on s).
--
-- running time: \(O(1)\)
extractOrd2 :: f (O s k) v -> O s (f k v)
extractOrd2 = unsafeCoerce

-- | Introduce dynamic order in a container 'f' that has keys of type
-- k.
--
-- running time: \(O(1)\)
introOrd2 :: f k v -> f (O s k) v
introOrd2 = unsafeCoerce
