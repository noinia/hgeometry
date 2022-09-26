--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Interval.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Intervals
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Interval.Boxed
  ( Interval(..)
  , ClosedInterval, OpenInterval
  ) where

import Control.Lens
import HGeometry.Interval.EndPoint
import HGeometry.Interval.Class
import HGeometry.Properties

--------------------------------------------------------------------------------
-- | Data type representing intervals
data Interval endPoint r = Interval !(endPoint r) !(endPoint r)
  deriving (Show,Eq,Ord)

type instance NumType (Interval endPoint r) = r

type ClosedInterval r = Interval (EndPoint Closed) r
type OpenInterval r   = Interval (EndPoint Open) r

instance Functor endPoint => Functor (Interval endPoint) where
  fmap f (Interval s t) = Interval (fmap f s) (fmap f t)
instance Foldable endPoint => Foldable (Interval endPoint) where
  foldMap f (Interval s t) = foldMap f s <> foldMap f t
instance Traversable endPoint => Traversable (Interval endPoint) where
  traverse f (Interval s t) = Interval <$> traverse f s <*> traverse f t

instance HasStart (Interval endPoint r) (endPoint r) where
  start = lens (\(Interval s _) -> s) (\(Interval _ t) s -> Interval s t)

instance HasEnd (Interval endPoint r) (endPoint r) where
  end   = lens (\(Interval _ t) -> t) (\(Interval s _) t -> Interval s t)

instance ( EndPoint_ (endPoint r), NumType (endPoint r) ~ r
         ) => Interval_ (Interval endPoint r) (endPoint r) where
  mkInterval = Interval


instance ClosedInterval_ (ClosedInterval r) r where
  mkClosedInterval s e = Interval (ClosedE s) (ClosedE e)

instance OpenInterval_ (OpenInterval r) r where
  mkOpenInterval s e = Interval (OpenE s) (OpenE e)


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
