--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Interval
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Intervals
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Interval
  ( Interval(..)
  , ClosedInterval, OpenInterval
  , module HGeometry.Interval.Class
  ) where

import Control.Lens
import HGeometry.Interval.EndPoint
import HGeometry.Interval.Class
import HGeometry.Properties

--------------------------------------------------------------------------------
-- | Data type representing intervals (using a boxed implementation)
data Interval endPoint r = Interval !(endPoint r) !(endPoint r)
  deriving (Show,Eq,Ord)

type instance NumType (Interval endPoint r) = r

-- | Cosed intervals (using a boxed representation)
type ClosedInterval r = Interval (EndPoint Closed) r

-- | Open intervals (using a boxed representation)
type OpenInterval r   = Interval (EndPoint Open) r

instance Functor endPoint => Functor (Interval endPoint) where
  fmap f (Interval s t) = Interval (fmap f s) (fmap f t)
instance Foldable endPoint => Foldable (Interval endPoint) where
  foldMap f (Interval s t) = foldMap f s <> foldMap f t
instance Traversable endPoint => Traversable (Interval endPoint) where
  traverse f (Interval s t) = Interval <$> traverse f s <*> traverse f t

instance ( EndPoint_ (endPoint r), IxValue (endPoint r) ~ r
         ) => HasStart (Interval endPoint r) r where
  start = startPoint._endPoint
instance ( EndPoint_ (endPoint r), IxValue (endPoint r) ~ r
         ) => HasEnd (Interval endPoint r) r where
  end = endPoint._endPoint

instance HasStartPoint (Interval endPoint r) (endPoint r) where
  startPoint = lens (\(Interval s _) -> s) (\(Interval _ t) s -> Interval s t)

instance HasEndPoint (Interval endPoint r) (endPoint r) where
  endPoint = lens (\(Interval _ t) -> t) (\(Interval s _) t -> Interval s t)

type instance EndPointOf (Interval endPoint r) = endPoint r

instance ( EndPoint_ (endPoint r), IxValue (endPoint r) ~ r
         ) => IntervalLike_ (Interval endPoint r) r where
  mkInterval = Interval

instance ( EndPoint_ (endPoint r), IxValue (endPoint r) ~ r
         ) => Interval_ (Interval endPoint r) r where


instance ClosedInterval_ (ClosedInterval r) r

instance OpenInterval_ (OpenInterval r) r

-- type instance IntersectionOf r (Interval endPoint r) = [NoIntersection, r]
-- GHC does not understand the r here cannot be 'Interval endPoint r' itself :(

-- type instance IntersectionOf (Interval point r) (Interval point r)
--   = [NoIntersection, Interval point r]

-- instance Ord r => Interval a r `HasIntersectionWith` Interval b r
-- instance Ord r => Interval point r `IsIntersectableWith` Interval point r where

--   nonEmptyIntersection = defaultNonEmptyIntersection

--   (GInterval r) `intersect` (GInterval s) = match (r' `intersect` s') $
--          H (\NoIntersection -> coRec NoIntersection)
--       :& H (\(Range l u)    -> coRec . GInterval $ Range (l&unEndPoint %~ g)
--                                                          (u&unEndPoint %~ g) )
--       :& RNil
--     where
--       r' :: Range (Arg r (r :+ Either a b))
--       r' = fmap (\(x :+ a) -> Arg x (x :+ Left a))  r
--       s' :: Range (Arg r (r :+ Either a b))
--       s' = fmap (\(x :+ b) -> Arg x (x :+ Right b)) s

--       g (Arg _ x) = x
