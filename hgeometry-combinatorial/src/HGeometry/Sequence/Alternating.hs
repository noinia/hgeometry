{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Sequence.Alternating
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Type representing Alternating sequences. The sequence type itself is parameterized.
--
--------------------------------------------------------------------------------
module HGeometry.Sequence.Alternating
  ( Alternating(..)
  , mapF
  , withNeighbours
  , mergeAlternating
  , insertBreakPoints
  , reverse

  , snocElemWith
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Bifoldable
import           Data.Bitraversable
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Functor.Apply ((<.*>))
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup.Traversable
import           GHC.Generics (Generic)
import           Prelude hiding (reverse)

--------------------------------------------------------------------------------

-- | A (non-empty) alternating sequence of @a@\'s and @sep@'s.
data Alternating f sep a = Alternating a (f (sep, a))
                         deriving (Generic)

deriving instance (Show a, Show (f (sep, a))) => Show (Alternating f sep a)
deriving instance (Eq a, Eq (f (sep, a)))     => Eq (Alternating f sep a)
deriving instance (Ord a, Ord (f (sep, a)))   => Ord (Alternating f sep a)

instance (NFData a, NFData (f (sep, a))) => NFData (Alternating f sep a)


instance Functor f => Functor (Alternating f sep) where
  fmap f (Alternating x xs) = Alternating (f x) $ fmap (fmap f) xs
instance Foldable f => Foldable (Alternating f sep) where
  foldMap f (Alternating x xs) = f x <> foldMap (f . snd) xs
instance Traversable f => Traversable (Alternating f sep) where
  traverse f (Alternating x xs) = Alternating <$> f x <*> traverse (traverse f) xs

instance Foldable f => Foldable1 (Alternating f sep) where
  foldMap1 f (Alternating x xs) = case foldMap (Just . f . snd) xs of
                                    Nothing -> f x
                                    Just r  -> f x <> r

instance Traversable f => Traversable1 (Alternating f sep) where
  traverse1 f (Alternating x xs) = Alternating <$> f x <.*> traverse1Maybe (traverse1 f) xs

instance Functor f => Bifunctor (Alternating f) where
  bimap f g (Alternating x xs) = Alternating (g x) $ fmap (bimap f g) xs

instance Foldable f => Bifoldable (Alternating f) where
  bifoldMap f g (Alternating x xs) = g x <> foldMap (bifoldMap f g) xs

instance Traversable f => Bitraversable (Alternating f) where
  bitraverse f g (Alternating x xs) = Alternating <$> g x <*> traverse (bitraverse f g) xs

-- | map some function changing the f into a g.
mapF                      :: (f (sep, a) -> g (sep', a))
                          -> Alternating f sep a -> Alternating g sep' a
mapF f (Alternating x xs) = Alternating x $ f xs


-- | Computes a b with all its neighbours
--
-- >>> withNeighbours (Alternating 0 [('a', 1), ('b', 2), ('c',3)])
-- [(0,'a',1),(1,'b',2),(2,'c',3)]
withNeighbours                       :: Foldable f => Alternating f sep a -> [(a,sep,a)]
withNeighbours xs@(Alternating _ ys) =
  zipWith (\a (s,a') -> (a,s,a')) (F.toList xs) (F.toList ys)


-- | Generic merging scheme that merges two Alternating Lists and applies the function
-- '@f@', with the current/new value at every event. So note that if the alternating
-- consists of 'Alternating a0 [(t1,a2)]' then the function is applied to a1, not to a0
-- (i.e. every value ai is considered alive on the interval [ti,t(i+1))
--
-- >>> let odds  = Alternating "a" [(3,"c"), (5,"e"), (7,"g")]
-- >>> let evens = Alternating "b" [(4,"d"), (6,"f"), (8,"h")]
-- >>> mergeAlternating (\_ a b -> a <> b) odds evens
-- [(3,"cb"),(4,"cd"),(5,"ed"),(6,"ef"),(7,"gf"),(8,"gh")]
-- >>> mergeAlternating (\t a b -> if t `mod` 2 == 0 then a else b) odds evens
-- [(3,"b"),(4,"c"),(5,"d"),(6,"e"),(7,"f"),(8,"g")]
-- >>> mergeAlternating (\_ a b -> a <> b) odds (Alternating "b" [(0,"d"), (5,"e"), (8,"h")])
-- [(0,"ad"),(3,"cd"),(5,"ee"),(7,"ge"),(8,"gh")]
mergeAlternating                         :: Ord t
                                         => (t -> a -> b -> c)
                                         -> Alternating [] t a -> Alternating [] t b
                                         -> [(t,c)]
mergeAlternating f (Alternating a00 as0)
                   (Alternating b00 bs0) = go a00 b00 as0 bs0
  where
    go a  _  []              bs               = map (\(t,b) -> (t, f t a b)) bs
    go _  b  as              []               = map (\(t,a) -> (t, f t a b)) as
    go a0 b0 as@((t, a):as') bs@((t', b):bs') = case t `compare` t' of
                                                      LT -> (t , f t  a  b0) : go a  b0 as' bs
                                                      EQ -> (t , f t  a  b)  : go a  b  as' bs'
                                                      GT -> (t', f t' a0 b)  : go a0 b  as  bs'

-- | Adds additional t-values in the alternating, (in sorted order). I.e. if we insert a
-- "breakpoint" at time t the current '@a@' value is used at that time.
--
-- >>> insertBreakPoints [0,2,4,6,8,10] $ Alternating "a" [(3, "c"), (5, "e"), (7,"g")]
-- Alternating "a" [(0,"a"),(2,"a"),(3,"c"),(4,"c"),(5,"e"),(6,"e"),(7,"g"),(8,"g"),(10,"g")]
insertBreakPoints                         :: Ord t
                                          => [t] -> Alternating [] t a -> Alternating [] t a
insertBreakPoints ts a@(Alternating a0 _) =
  Alternating a0 $ mergeAlternating (\_ _ a' -> a') (Alternating undefined (map (,()) ts)) a


-- | Reverses an alternating list.
--
-- >>> reverse $ Alternating "a" [(3, "c"), (5, "e"), (7, "g")]
-- Alternating "g" [(7,"e"),(5,"c"),(3,"a")]
reverse                      :: Alternating [] b a -> Alternating [] b a
reverse p@(Alternating s xs) = case NonEmpty.nonEmpty xs of
    Nothing               -> p
    Just xs1@((e1,_):|tl) -> let ys    = (e1, s) : List.zipWith (\(_, v) (e, _) -> (e, v)) xs tl
                                 (_,t) = NonEmpty.last xs1
                             in Alternating t (List.reverse ys)


-- | Given a function f that takes the (current) last element x, and the new element y,
-- and computes the new separating element s, snocs the separator and y onto the
-- alternating list.
snocElemWith                         :: Snoc (f (sep,a)) (f (sep,a)) (sep,a) (sep,a)
                                     => (a -> a -> sep)
                                     -> Alternating f sep a -> a -> Alternating f sep a
snocElemWith f (Alternating x0 xs) y = Alternating x0 $ view (re _Snoc) (xs, (s,y))
  -- a 're _Snoc' is essentially something that when given a tuple (zs,z) turns it into a
  -- zs `snoc` z
  where
    s = case xs^?_last of
          Nothing    -> f x0 y
          Just (_,x) -> f x  y
