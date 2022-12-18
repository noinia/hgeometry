{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
module HGeometry.Cyclic
  ( Cyclic(..), _CyclicVector, toCircularVector

  , AsCyclic(..)
  ) where

--------------------------------------------------------------------------------

import           Control.DeepSeq (NFData)
import           Control.Lens
import qualified Data.Foldable as F
import           Data.Semigroup.Foldable
import           Data.Vector.Circular (CircularVector(..))
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Generic as GV
import           Data.Vector.NonEmpty (NonEmptyVector)
import           GHC.Generics (Generic)
import           HGeometry.Foldable.Util

--------------------------------------------------------------------------------

-- | A cyclic sequence type
newtype Cyclic v a = Cyclic (v a)
 deriving newtype (Functor,Foldable,NFData)
 deriving (Generic)

instance Foldable1 v    => Foldable1    (Cyclic v)

instance Traversable1 v => Traversable1 (Cyclic v) where
  traverse1 f (Cyclic v) = Cyclic <$> traverse1 f v
instance Traversable v => Traversable (Cyclic v) where
  traverse f (Cyclic v) = Cyclic <$> traverse f v


instance FunctorWithIndex i v => FunctorWithIndex i (Cyclic v) where
  imap f (Cyclic v) = Cyclic $ imap f v
instance FoldableWithIndex i v => FoldableWithIndex i (Cyclic v) where
  ifoldMap f (Cyclic v) = ifoldMap f v
instance TraversableWithIndex i v => TraversableWithIndex i (Cyclic v) where
  itraverse f (Cyclic v) = Cyclic <$> itraverse f v

instance HasFromFoldable v => HasFromFoldable (Cyclic v)  where
  fromFoldable = Cyclic . fromFoldable
  fromList = Cyclic . fromList

instance HasFromFoldable1 v => HasFromFoldable1 (Cyclic v)  where
  fromFoldable1 = Cyclic . fromFoldable1
  fromNonEmpty  = Cyclic . fromNonEmpty

type instance Index   (Cyclic v a) = Index   (v a)
type instance IxValue (Cyclic v a) = IxValue (v a)

instance (Index (v a) ~ Int, Foldable v, Ixed (v a)) => Ixed (Cyclic v a) where
  ix i = \f (Cyclic v) -> let n = F.length v
                          in Cyclic <$> ix (i `mod` n) f v

-- | Get the underlying vector
_CyclicVector :: Iso (Cyclic v a) (Cyclic w b) (v a) (w b)
_CyclicVector = iso (\(Cyclic v) -> v) Cyclic

-- | Turn the cyclic vector into a circular Vector
toCircularVector            :: Cyclic NonEmptyVector a -> CircularVector a
toCircularVector (Cyclic v) = CircularVector v 0

class AsCyclic v where
  -- | Given an index i, returns an "rightwards" (i.e. clockwise)
  -- IndexedFold of the cyclic structure, starting from the
  -- element with index i.
  ifoldRightFrom :: Int -> IndexedFold Int (Cyclic v a) a

  -- | Given an index i, returns an "leftward" (i.e. count clockwise)
  -- IndexedFold of the cyclic structure, starting from the
  -- element with index i.
  ifoldLeftFrom :: Int -> IndexedFold Int (Cyclic v a) a


--  {-# MINIMAL itraverseRightFrom, itraverseLeftFrom #-}
  -- -- | Given an index i, returns an "rightwards" (i.e. clockwise)
  -- -- IndexedTraversal of the cyclic structure, starting from the
  -- -- element with index i.
  -- itraverseRightFrom :: Int -> IndexedTraversal Int (Cyclic v a) (Cyclic v b) a b

  -- -- | Given an index i, returns an "leftwards" (i.e. counter
  -- -- clockwise) IndexedTraversal of the cyclic structure, starting
  -- -- from the element with index i.
  -- itraverseLeftFrom   :: Int -> IndexedTraversal Int (Cyclic v a) (Cyclic v b) a b
  -- -- itraverseLeftFrom i =

instance (forall a. GV.Vector v a) => AsCyclic v where
  ifoldRightFrom s = _CyclicVector.ifoldRightFromV s
  {-# INLINE ifoldRightFrom #-}
  ifoldLeftFrom  s = _CyclicVector.ifoldLeftFromV s
  {-# INLINE ifoldLeftFrom #-}

-- | implementation of iFoldRight for vectors
ifoldRightFromV   :: GV.Vector v a => Int -> IndexedFold Int (v a) a
ifoldRightFromV s = conjoined (foldRightFrom' s) (ifoldRightFrom' s)
{-# INLINE ifoldRightFromV #-}

-- | implementation of iFoldLeft for vectors
ifoldLeftFromV   :: GV.Vector v a => Int -> IndexedFold Int (v a) a
ifoldLeftFromV s = conjoined (foldLeftFrom' s) (ifoldLeftFrom' s)
{-# INLINE ifoldLeftFromV #-}

--------------------------------------------------------------------------------
-- * ifoldRight implementation for vectors

-- | foldRight, i.e. without the index.
foldRightFrom'   :: GV.Vector v a => Int -> Fold (v a) a
foldRightFrom' s = foldring foldrF
  where
    -- foldrF :: (Contravariant f, Applicative f) => (a -> f a -> f a) -> f a -> s -> f a
    foldrF k z v = Bundle.foldr k (Bundle.foldr k z v2) v1
      where
        v1 = GV.stream $ GV.slice s (n - s) v
        v2 = GV.stream $ GV.slice 0 s       v
        !n = GV.length v
{-# INLINE foldRightFrom' #-}

-- | Fold the elements of the vector starting from the element at
-- index s, going rightwards.
ifoldRightFrom'   :: GV.Vector v a => Int -> IndexedFold Int (v a) a
ifoldRightFrom' s = ifoldring foldrF
  where
    -- foldrF :: (Indexable i p, Contravariant f, Applicative f) => ((i -> a -> f a -> f a) -> f a -> s -> f a) -> Over p f s t a b
    foldrF k z v = Bundle.foldr k' (Bundle.foldr k' z v2) v1
      where
        k' (i,x) = k i x
        v1 = fmap (\(i,x) -> (s+i,x))
           . Bundle.indexed . GV.stream $ GV.slice s (n - s) v
        v2 = Bundle.indexed . GV.stream $ GV.slice 0 s       v
        !n = GV.length v
{-# INLINE ifoldRightFrom' #-}

--------------------------------------------------------------------------------
-- * iFoldLeft implementation for vectors

-- | foldLeftFrom for vectors
foldLeftFrom'   :: GV.Vector v a => Int -> Fold (v a) a
foldLeftFrom' s = foldring foldrF
  where
    -- foldrF :: (Contravariant f, Applicative f) => (a -> f a -> f a) -> f a -> s -> f a
    foldrF k z v = Bundle.foldr k (Bundle.foldr k z v1) v2
      where
        v1 = GV.streamR $ GV.slice (s+1) (n - s - 1) v
        v2 = GV.streamR $ GV.slice 0 (s+1)           v
        !n = GV.length v
{-# INLINE foldLeftFrom' #-}

-- | Fold the elements of the vector starting from the element at
-- index s, going rightwards.
ifoldLeftFrom'   :: GV.Vector v a => Int -> IndexedFold Int (v a) a
ifoldLeftFrom' s = ifoldring foldrF
  where
    foldrF k z v = Bundle.foldr k' (Bundle.foldr k' z v1) v2
      where
        k' (i,x) = k i x
        v1 = fmap (\(i,x) -> (n-i,x))
           . Bundle.indexed . GV.streamR $ GV.slice (s+1) (n - s - 1) v
        v2 = fmap (\(i,x) -> (s+1-i,x))
           . Bundle.indexed . GV.streamR $ GV.slice 0 (s+1)           v
        !n = GV.length v
{-# INLINE ifoldLeftFrom' #-}


--------------------------------------------------------------------------------
{-

-- I guess the traversals don't make much sense; i.e. only when they are actually monadic.
-- otherwise the order in which we do the traversal shouldn't matter.

-- itraverseRightFrom   :: (GV.Vector v a, GV.Vector v' b)
--                      => Int -> IndexedTraversal Int (v a) (v' b) a b
-- itraverseRightFrom s = conjoined (traverseRightFrom' s) (itraverseRightFrom s)

traverseRightFrom'   :: forall v v' a b. (GV.Vector v a, GV.Vector v' b)
                     => Int -> Traversal (v a) (v' b) a b
traverseRightFrom' s f v = (\xs1 xs2 -> GV.fromListN n (xs2 <> xs1)) <$> v1 <*> v2
  where
    v1 = traverse f . GV.toList $ GV.slice s (n - s) v
    v2 = traverse f . GV.toList $ GV.slice 0 s       v
    !n = GV.length v

itraverseRightFrom'          :: (GV.Vector v a, GV.Vector v' b)
                             => Int -> IndexedTraversal Int (v a) (v' b) a b
itraverseRightFrom' s paFb v = (\xs1 xs2 -> GV.fromListN n (xs2 <> xs1)) <$> v1 <*> v2
  where
    v1 = (reindexed (+s) itraversed) paFb . GV.toList $ GV.slice s (n - s) v
    v2 = itraversed                  paFb . GV.toList $ GV.slice 0 s       v
    !n = GV.length v


--------------------------------------------------------------------------------

traverseLeftFrom'   :: forall v v' a b. (GV.Vector v a, GV.Vector v' b)
                     => Int -> Traversal (v a) (v' b) a b
traverseLeftFrom' s f v = (\xs1 xs2 -> GV.fromListN n (xs2 <> xs1)) <$> v1 <*> v2
  where
    v1 = traverse f . Bundle.toList . GV.streamR $ GV.slice (s+1) (n - s -1) v
    v2 = traverse f . Bundle.toList . GV.streamR $ GV.slice 0     (s+1)      v
    !n = GV.length v

itraverseLeftFrom'          :: (GV.Vector v a, GV.Vector v' b)
                             => Int -> IndexedTraversal Int (v a) (v' b) a b
itraverseLeftFrom' s paFb v = (\xs1 xs2 -> GV.fromListN n (xs2 <> xs1)) <$> v1 <*> v2
  where
    v1 = (reindexed (\i -> n-i)  itraversed) paFb
       . Bundle.toList . GV.streamR $ GV.slice s (n - s) v
    v2 = (reindexed (\i -> s+1-i) itraversed) paFb
       . Bundle.toList . GV.streamR $ GV.slice 0 s       v
    !n = GV.length v

--------------------------------------------------------------------------------

-- itraverseRightFrom'   :: (GV.Vector v a, GV.Vector v' b) => IndexedTraversal Int (v a) (v' b) a b
-- itraverseRightFrom' s = undefined



-- toiListFrom   :: GV.Vector v a => Int -> IndexedFold Int (v a) a
-- toiListFrom s = ifoldring foldrF
--   where
--     foldrF = Bundle.foldr



-- instance AsCyclic NonEmptyVector where
--   itraverseRightFrom i pafb (Cyclic v) = Cyclic <$> do v' <-

--     [0..n]
--     where
--       n = length cv
-}

-- test :: V.Vector Char
-- test = V.fromList $ ['a' .. 'g']
