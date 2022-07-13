{-# LANGUAGE StandaloneKindSignatures #-}
module Geometry.LineSegment.Class
  ( Interval_(..)
  , Interval(Interval)


  , LineSegment_
  ) where

import Control.Lens
import Data.Kind
import GHC.TypeNats
import Geometry.Boundary
import Geometry.Properties
-- import Data.Range (EndPoint(..))

--------------------------------------------------------------------------------

class HasStart seg p where
  start :: Lens' seg p

class HasEnd seg p where
  end :: Lens' seg p

-- type Interval_ :: ( (Type -> Type) -> Type -> Type )
--                -> (Type -> Type) -- ^ the endpoint
--                -> Type    -- ^ the numeric type
--                -> Constraint
class ( HasStart (interval endPoint r) (endPoint r)
      , HasEnd   (interval endPoint r) (endPoint r)
      , EndPoint_ endPoint
      ) => Interval_ interval endPoint r  where




--------------------------------------------------------------------------------

inInterval       :: forall interval endPoint r. (Ord r, Interval_ interval endPoint r)
                 => r -> interval endPoint r -> PointLocationResult
x `inInterval` i =
    case x `compare` (i^.start.endPoint @endPoint) of
      LT -> Outside
      EQ -> OnBoundary
      GT -> case x `compare` (i^.end.endPoint @endPoint) of
              LT -> Inside
              EQ -> OnBoundary
              GT -> Outside




--------------------------------------------------------------------------------

data Interval endPoint r = Interval !(endPoint r) !(endPoint r)

data EndPointType = OpenEndPoint | ClosedEndPoint deriving (Show,Eq)

class EndPoint_ endPoint where
  endPoint     :: Lens (endPoint r) (endPoint r') r r'
  endPointType :: endPoint r -> EndPointType

----------------------------------------

data IntervalEndPoint r = IntervalEndPoint EndPointType r
                        deriving (Show,Eq)

instance EndPoint_ IntervalEndPoint where
  endPoint     =
    lens (\(IntervalEndPoint _ p) -> p) (\(IntervalEndPoint t _) p -> IntervalEndPoint t p)
  endPointType (IntervalEndPoint t _) = t




--------------------------------------------------------------------------------



instance HasStart (Interval endPoint r) (endPoint r) where
  start = lens (\(Interval s _) -> s) (\(Interval _ t) s -> Interval s t)

instance HasEnd (Interval endPoint r) (endPoint r) where
  end   = lens (\(Interval _ t) -> t) (\(Interval s _) t -> Interval s t)

instance EndPoint_ endPoint => Interval_ Interval endPoint r where




--------------------------------------------------------------------------------

-- data SegmentEndPoint point d r = SegmentEndPoint EndPointType (point d r)
--                         deriving (Show,Eq)


class ( HasStart (lineSegment d point r) (point d r)
      , HasEnd   (lineSegment d point r) (point d r)

      ) => LineSegment_ lineSegment d point r where



newtype LineSegmentF endPoint d point r =
  MkLineSegment (Interval endPoint (point d r))



_WrappedInterval :: Iso (LineSegment d point r)         (LineSegment d' point' r')
                        (Interval IntervalEndPoint (point d r)) (Interval IntervalEndPoint (point' d' r'))
_WrappedInterval = iso (\(MkLineSegment i) -> i) MkLineSegment



instance HasStart (LineSegment d point r) (point d r) where
  start = _WrappedInterval.start.endPoint @IntervalEndPoint

instance HasEnd   (LineSegment d point r) (point d r) where
  end   = _WrappedInterval.end.endPoint @IntervalEndPoint


instance LineSegment_ LineSegment d point r where


--------------------------------------------------------------------------------

newtype Closed r = Closed r deriving (Show,Eq,Ord)

instance EndPoint_ Closed where
  endPoint = lens (\(Closed x) -> x) (\_ x -> Closed x)
  endPointType = const ClosedEndPoint

newtype ClosedLineSegment d point r = MkClosedLineSegment (Interval (point d) r)

-- _WrappedClosedInterval :: Iso (ClosedLineSegment d point r) (ClosedLineSegment d' point' r')



--                           (Interval IntervalEndPoint (point d r)) (Interval IntervalEndPoint (point' d' r'))
-- _WrappedClosedInterval = iso (\(MkClosedLineSegment i) -> i) MkClosedLineSegment
