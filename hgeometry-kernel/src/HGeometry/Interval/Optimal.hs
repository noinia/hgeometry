--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Interval.Optimal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Intervals
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Interval.Optimal
  ( Interval(Interval, ClosedInterval, OpenInterval)
  , ClosedInterval, OpenInterval
  ) where

import Control.Lens
import HGeometry.Interval.EndPoint
import HGeometry.Interval.Class
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------
-- | Data type representing intervals
newtype Interval endPoint r = MkInterval (Vector 2 (endPoint r))

type ClosedInterval r = Interval (EndPoint Closed) r
type OpenInterval r   = Interval (EndPoint Open) r


-- | Construct an interval
pattern Interval     :: OptCVector_ 2 (endPoint r)
                     =>  endPoint r -> endPoint r -> Interval endPoint r
pattern Interval s t = MkInterval (Vector2 s t)
{-# COMPLETE Interval #-}

-- | Construct a closed interval
pattern ClosedInterval     :: OptCVector_ 2 r
                           =>  r -> r -> ClosedInterval r
pattern ClosedInterval s t = Interval (ClosedE s) (ClosedE t)

-- | Construct an open ended interval
pattern OpenInterval     :: OptCVector_ 2 r
                         =>  r -> r -> OpenInterval r
pattern OpenInterval s t = Interval (OpenE s) (OpenE t)

type instance NumType   (Interval endPoint r) = r
type instance Dimension (Interval endPoint r) = 1

-- TODO fix, this is clearly not how we want to store an interval ....
deriving instance (Show (endPoint r), OptVector_ 2 (endPoint r)) => Show (Interval endPoint r)


-- it is not so nice that we cannot derrive those:
instance (Eq (endPoint r), OptCVector_ 2 (endPoint r)) => Eq (Interval endPoint r) where
  (Interval s t) == (Interval s' t') = s == s' && t == t'

instance (Ord (endPoint r), OptCVector_ 2 (endPoint r)) => Ord (Interval endPoint r) where
  (Interval s t) `compare` (Interval s' t') = s `compare` s' <> t `compare` t'

instance OptCVector_ 2 (endPoint r) => HasStart (Interval endPoint r) (endPoint r) where
  start = lens (\(Interval s _) -> s) (\(Interval _ t) s -> Interval s t)
instance OptCVector_ 2 (endPoint r) => HasEnd (Interval endPoint r) (endPoint r) where
  end = lens (\(Interval _ t) -> t) (\(Interval s _) t -> Interval s t)

instance ( OptCVector_ 2 (endPoint r), EndPoint_ (endPoint r), NumType (endPoint r) ~ r
         ) => Interval_ (Interval endPoint r) (endPoint r) where
  mkInterval = Interval
instance OptCVector_ 2 r => ClosedInterval_ (ClosedInterval r) r where
  mkClosedInterval = ClosedInterval
instance OptCVector_ 2 r => OpenInterval_ (OpenInterval r) r where
  mkOpenInterval = OpenInterval


  -- (Show,Eq,Ord)

test :: ClosedInterval Int
test = ClosedInterval 5 10


{-


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


-}




--------------------------------------------------------------------------------
