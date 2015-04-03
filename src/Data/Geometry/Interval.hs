{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.Interval(
                             -- * 1 dimensional Intervals
                               Interval(..)
                             , Intersection(..)

                             -- * querying the start and end of intervals
                             , HasStart(..), HasEnd(..)
                             -- * Working with intervals
                             , width
                             , inInterval
                             ) where

import           Control.Lens
import           Data.Ext
import           Data.Geometry.Properties

--------------------------------------------------------------------------------

data Interval a r = Interval { _start :: r :+ a
                             , _end   :: r :+ a
                             }
                  deriving (Show,Read,Eq,Functor)


class HasStart t where
  type StartCore t
  type StartExtra t
  start :: Lens' t (StartCore t :+ StartExtra t)

instance HasStart (Interval a r) where
  type StartCore (Interval a r) = r
  type StartExtra (Interval a r) = a
  start = lens _start (\(Interval _ e) s -> Interval s e)

class HasEnd t where
  type EndCore t
  type EndExtra t
  end :: Lens' t (EndCore t :+ EndExtra t)

instance HasEnd (Interval a r) where
  type EndCore (Interval a r) = r
  type EndExtra (Interval a r) = a
  end = lens _end (\(Interval s _) e -> Interval s e)


-- | When comparing intervals, compare them lexicographically on
-- (start^.core,end^.core,start^.extra,end^.extra)
instance (Ord a, Ord r) => Ord (Interval a r) where
  (Interval (s :+ a) (t :+ b)) `compare` (Interval (p :+ c) (q :+ d)) =
    (s,t,a,b) `compare` (p,q,c,d)

type instance Dimension (Interval a r) = 1
type instance NumType   (Interval a r) = r


instance Ord r => IsIntersectableWith (Interval a r) (Interval a r) where

  data Intersection (Interval a r) (Interval a r) = IntervalIntersection (Interval a r)
                                                  | NoOverlap
                                                  deriving (Show,Read,Eq,Ord)

  nonEmptyIntersection NoOverlap = False
  nonEmptyIntersection _         = True

  (Interval a b) `intersect` (Interval c d)
      | s^.core <= t^.core = IntervalIntersection $ Interval s t
      | otherwise          = NoOverlap
    where

      s = a `maxOnCore` c
      t = b `minOnCore` d


instance Ord r => IsUnionableWith (Interval a r) (Interval a r) where

  data Union (Interval a r) (Interval a r) = DisjointIntervals (Interval a r) (Interval a r)
                                           | OneInterval       (Interval a r)
                                           deriving (Show,Read,Eq,Ord)

  i@(Interval a b) `union` j@(Interval c d)
      | i `intersects` j = OneInterval $ Interval s t
      | otherwise        = DisjointIntervals i j
    where
      s = a `minOnCore` c
      t = b `maxOnCore` d


maxOnCore :: Ord c => c :+ e -> c :+ e -> c :+ e
l@(lc :+ _) `maxOnCore` r@(rc :+ _) = if lc >= rc then l else r

minOnCore :: Ord c => c :+ e -> c :+ e -> c :+ e
l@(lc :+ _) `minOnCore` r@(rc :+ _) = if lc <= rc then l else r


-- | Get the width of the interval
width   :: Num r => Interval a r -> r
width i = i^.end.core - i^.start.core

inInterval       :: Ord r => r -> Interval a r -> Bool
x `inInterval` i = i^.start.core <= x && x <= i^.end.core
