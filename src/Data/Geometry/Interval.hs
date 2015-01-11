{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.Interval( Interval(..)
                             ) where

import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Monoid
import           Data.Geometry.Properties


--------------------------------------------------------------------------------

data Interval a r = Interval { _start :: r :+ a
                             , _end   :: r :+ a
                             }
                  deriving (Show,Read,Eq,Functor)

makeLenses ''Interval

-- | When comparing intervals, compare them lexicographically on
-- (start^.core,end^.core,start^.extra,end^.extra)
instance (Ord a, Ord r) => Ord (Interval a r) where
  (Interval (s :+ a) (t :+ b)) `compare` (Interval (p :+ c) (q :+ d)) =
    (s,t,a,b) `compare` (p,q,c,d)

type instance Dimension (Interval a r) = 1
type instance NumType   (Interval a r) = r


instance (Ord r) => IsIntersectableWith (Interval a r) (Interval a r) where

  data Intersection (Interval a r) (Interval a r) = IntervalIntersection (Interval a r)
                                                  | NoOverlap
                                                  deriving (Show,Read,Eq,Ord)

  (Interval a b) `intersect` (Interval c d)
      | s^.core <= t^.core = IntervalIntersection $ Interval s t
      | otherwise          = NoOverlap
    where

      s = a `maxOnCore` c
      t = b `minOnCore` d



l@(lc :+ _) `maxOnCore` r@(rc :+ _) = if lc >= rc then l else r

l@(lc :+ _) `minOnCore` r@(rc :+ _) = if lc <= rc then l else r
