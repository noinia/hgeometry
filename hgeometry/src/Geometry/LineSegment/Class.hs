{-# LANGUAGE StandaloneKindSignatures #-}
module Geometry.LineSegment.Class
  ( Interval_(..)
  , Interval(Interval)


  , LineSegment
  ) where

import Control.Lens
import Data.Kind
-- import Data.Range (EndPoint(..))



-- class LineSegment_ lineSegment d point r where
--   start :: Lens' (lineSegment d point r) (point d r)
--   end   :: Lens' (lineSegment d point r) (point d r)

-- class HasStart s t a b where
--   start :: Lens s t a b

-- class HasEnd s t a b where
--   end :: Lens s t a b
type Interval_ :: ( (Type -> Type) -> Type -> Type )
               -> (Type -> Type) -- ^ the endpoint
               -> Type    -- ^ the numeric type
               -> Constraint
class Interval_ (interval) endPoint r where
  start :: Lens' (interval endPoint r) (endPoint r)
  end   :: Lens' (interval endPoint r) (endPoint r)


data Interval endPoint r = Interval !(endPoint r) !(endPoint r)


data EndPoint point d r = EndPoint EndPointType (point d r)
                        deriving (Show,Eq)

data EndPointType = Open | Closed deriving (Show,Eq)


instance Interval_ Interval endPoint r where
  start = lens (\(Interval s _) -> s) (\(Interval _ t) s -> Interval s t)
  end   = lens (\(Interval _ t) -> t) (\(Interval s _) t -> Interval s t)





newtype LineSegment d point r = MkLineSegment (Interval (EndPoint point d) r)

_WrappedInterval :: Iso (LineSegment d point r)         (LineSegment d' point' r')
                        (Interval (EndPoint point d) r) (Interval (EndPoint point' d') r')
_WrappedInterval = iso (\(MkLineSegment i) -> i) MkLineSegment


-- instance Interval_ (LineSegment d) (point d) r where
--   start = _WrappedInterval . start
--   end   = _WrappedInterval . end



-- newtype Interval point1 r = MkInterval (LineSegment )

-- class LineSegment_ lineSegment



-- class Interval_ lineSegment (point d) r => LineSegment_ lineSegment d point r






-- instance Interval_ (LineSegment d) point r where
--   start = lens (\(LineSegment s _) -> s) (\(LineSegment _ t) s -> LineSegment s t)

-- instance LineSegment_ LineSegment d point r where
