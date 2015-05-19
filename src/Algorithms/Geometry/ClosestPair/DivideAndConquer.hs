{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies  #-}
module Algorithms.Geometry.ClosestPair.DivideAndConquer where

import           Algorithms.Geometry.ClosestPair.Naive(Two(..), PP, mkPair, getVal)
import           Control.Applicative
import           Control.Lens((^.))
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry(qdA)
import           Data.Geometry.Point
import           Data.Geometry.Vector
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr
import qualified Data.Vector.Fixed as FV


-- A VS represents a vector of d views on the same data. We use it to maintain
-- a set a sorted on d different keys.
type VS d a = Vector d (S.Seq a)


-- -- | Split the VS using the i^th index, at a given position j. Returns a triple
-- -- (l,x,r) with:
-- -- all elements in l on positions < j (according to the i^th index)
-- -- al elements in r on positions >= j (according to the i^th index)
-- -- x = the element on the j^th position according to index i (i..e)
-- split        :: (i <=. d) => VS d a -> proxy i -> Int -> (a -> Bool)

--                 (VS d a, a, VS d a)
-- split vs i p = (l,x,r)
--   where
--     l = filter' p
--     r = filter' p
--     p y =
--     (S.ViewL x _) = s



filter'   :: Arity d => (a -> Bool) -> VS d a -> VS d a
filter' p = fmap (S.filter p)


half      :: (1 <=. d, i <=. d, Arity d)
             => VS d (Point d r :+ p) -> proxy i ->
             ( VS d (Point d r :+ p)
             , Point d r :+ p
             , VS d (Point d r :+ p)
             )
half vs i = (filter' pred vs, p, filter (not . pred) vs)
  where
    h = size vs `div` 2
    p = (elemAt i h vs)
    pred q = (q^.core.coord i)  < (p^.core.coord i)


elemAt        :: (Arity d, i <=. d) => proxy i -> Int -> (VS d a) -> a
elemAt i j vs = S.index j $ vs^.element i


size    :: (1 <=. d, Arity d) => VS d a -> Int
size vs = S.length $ vs^.element (C :: C 0)



-- | divide and conquer algo for 2 dimensional closest pair
-- Note that we need at least two elements
-- for there to be a closest pair.
closestPair :: ( FV.Dim v ~ FV.S (FV.S n)
               , FV.Vector v (Point 2 r :+ p)
               , Ord r, Num r
               ) => v (Point 2 r :+ p) -> Two (Point 2 r :+ p)
closestPair = undefined
  -- where



closestPair'    :: (1 <=. d', Arity d')
                => VS d (Point d r :+ p) -> Two (Point 2 r :+ p)
closestPair' vs = undefined
  where
    (l,x,r) = half vs (C :: C 1)



-- newtype Count = Count Int deriving (Show,Eq,Ord,Num,Integral,Real,Enum)

-- instance Semigroup Count where
--   a <> b = a + b

-- instance Monoid Count where
--   mempty = 1
--   a `mappend` b = a <> b

-- data Lab a = Lab !(Max a) !Count


-- data Tree v a = Leaf a
--               | Node (Tree v a) v (Tree v a)
--               deriving (Show,Eq)

-- node     :: (Measure v a, Semigroup v) => Tree v a -> Tree v a -> Tree v a
-- node l r = Node l (measure l <> measure r) r

-- class Semigroup v => Measure v a | a -> v where
--   measure :: a -> v

-- instance Measure v a => Measure v (Tree v a) where
--   measure (Leaf a)     = measure a
--   measure (Node _ v _) = v

-- instance Measure v a => Semigroup (Tree v a) where
--   (<>) = node


-- traverse'  :: (Measure v b, Semigroup v, Applicative f) =>
--               (a -> f b) -> Tree u a -> f (Tree v b)
-- traverse' f (Leaf a)     = Leaf <$> f a
-- traverse' f (Node l _ r) = node <$> traverse' f l <*> traverse' f r


-- -- annotate :: BinLeafTree a -> Tree (Lab a) a
-- -- annotate =  traverse (\x -> Leaf $ Lab (Max x mempty))
