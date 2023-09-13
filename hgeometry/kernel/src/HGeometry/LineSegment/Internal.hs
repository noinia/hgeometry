{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LineSegment.Internal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Line segment data type and some basic functions on line segments
--
--------------------------------------------------------------------------------
module HGeometry.LineSegment.Internal
  ( LineSegment(LineSegment, ClosedLineSegment, OpenLineSegment)
  , ClosedLineSegment
  , OpenLineSegment
  , EndPoint(EndPoint,OpenE,ClosedE)
  , AnEndPoint(AnEndPoint,AnOpenE,AnClosedE)
  , module HGeometry.LineSegment.Class
  , EndPoint_(..)
  ) where


import Control.Lens
import Data.Functor.Apply
import Data.Kind (Type)
import HGeometry.Box.Boxable
import HGeometry.Intersection
import HGeometry.Interval.Class
import HGeometry.Interval
import HGeometry.Line.Class
import HGeometry.Line.PointAndVector
import HGeometry.LineSegment.Class
import HGeometry.Point
import HGeometry.Properties (NumType, Dimension)
-- import HGeometry.Transformation
import HGeometry.Vector
import Text.Read

-- import HGeometry.Number.Real.Rational

--------------------------------------------------------------------------------

-- >>> :{
-- testseg :: ClosedLineSegment (Point 2 Double)
-- testseg = ClosedLineSegment (Point2 5.0 6.0) (Point2 10.0 10.0)
--
-- myPoint :: Point 2 Double
-- myPoint = interpolate 0.5 testseg
-- :}



-- testI :: ClosedInterval (Point 2 Double)
-- testI = ClosedInterval (Point2 5.0 6.0) (Point2 10.0 10.0)






-- | Data type representing intervals
type LineSegment :: (Type -> Type) -> Type -> Type
newtype LineSegment endPoint point = MkLineSegment (Interval endPoint point)

-- | Default implementation of Closed LineSegments
type ClosedLineSegment point = LineSegment (EndPoint Closed) point

-- | Default implementation of Open LineSegments
type OpenLineSegment point   = LineSegment (EndPoint Open) point

-- | Construct a line Segment
pattern LineSegment     :: endPoint point -> endPoint point -> LineSegment endPoint point
pattern LineSegment p q = MkLineSegment (Interval p q)
{-# COMPLETE LineSegment #-}

-- | Construct a closed interval
pattern ClosedLineSegment     :: point -> point -> ClosedLineSegment point
pattern ClosedLineSegment s t = LineSegment (ClosedE s) (ClosedE t)
{-# COMPLETE ClosedLineSegment #-}

-- | Construct an open ended interval
pattern OpenLineSegment     :: point -> point -> OpenLineSegment point
pattern OpenLineSegment s t = LineSegment (OpenE s) (OpenE t)
{-# COMPLETE OpenLineSegment #-}

type instance NumType   (LineSegment endPoint point) = NumType point
type instance Dimension (LineSegment endPoint point) = Dimension point

deriving instance (Eq (endPoint point)--, OptCVector_ 2 (endPoint point)
                  ) => Eq (LineSegment endPoint point)
deriving instance (Ord (endPoint point)--, OptCVector_ 2 (endPoint point)
                  ) => Ord (LineSegment endPoint point)


instance Foldable endPoint => Foldable (LineSegment endPoint) where
  foldMap f (LineSegment s t) = foldMap f s <> foldMap f t

instance Functor endPoint => Functor (LineSegment endPoint) where
  fmap f (LineSegment s t) = LineSegment (fmap f s) (fmap f t)

instance Traversable endPoint => Traversable (LineSegment endPoint) where
  traverse f (LineSegment s t) = LineSegment <$> traverse f s <*> traverse f t


-- | Lens to get the underlying interval
_LineSegmentInterval :: Iso (LineSegment endPoint point) (LineSegment endPoint' point')
                            (Interval endPoint point)    (Interval    endPoint' point')
_LineSegmentInterval = iso (\(MkLineSegment i) -> i) MkLineSegment

instance ( IxValue (endPoint point) ~ point
         -- , OptCVector_ 2 (endPoint point)
         , EndPoint_ (endPoint point)
         ) => HasStart (LineSegment endPoint point) point where
  start = _LineSegmentInterval.start

instance ( IxValue (endPoint point) ~ point
         -- , OptCVector_ 2 (endPoint point)
         ) => HasStartPoint (LineSegment endPoint point) (endPoint point) where
  startPoint = _LineSegmentInterval.startPoint


instance ( IxValue (endPoint point) ~ point
         -- , OptCVector_ 2 (endPoint point)
         , EndPoint_ (endPoint point)
         ) => HasEnd (LineSegment endPoint point) point where
  end = _LineSegmentInterval.end

instance ( IxValue (endPoint point) ~ point
         -- , OptCVector_ 2 (endPoint point)
         ) => HasEndPoint (LineSegment endPoint point) (endPoint point) where
  endPoint = _LineSegmentInterval.endPoint

type instance StartPointOf (LineSegment endPoint point) = endPoint point
type instance EndPointOf   (LineSegment endPoint point) = endPoint point

instance ( IxValue (endPoint point) ~ point
         -- , OptCVector_ 2 (endPoint point)
         , EndPoint_ (endPoint point)
         ) => IntervalLike_ (LineSegment endPoint point) point where
  mkInterval = LineSegment

instance ( IxValue (endPoint point) ~ point
         -- , OptCVector_ 2 (endPoint point)
         , EndPoint_ (endPoint point)
         , Point_ point (Dimension point) (NumType point)
         ) => LineSegment_ (LineSegment endPoint point) point where
  uncheckedLineSegment s t = LineSegment (mkEndPoint s) (mkEndPoint t)

instance ( --OptCVector_ 2 (EndPoint Closed point)
--         ,
         Point_ point (Dimension point) (NumType point)
         ) => ClosedLineSegment_ (ClosedLineSegment point) point where

instance ( -- OptCVector_ 2 (EndPoint Open point)
--         ,
         Point_ point (Dimension point) (NumType point)
         ) => OpenLineSegment_ (OpenLineSegment point) point where

instance ( -- OptCVector_ 2 (endPoint point)
--         , OptCVector_ 2 (endPoint point')
--         ,
          Traversable1 endPoint
         , Dimension point ~ Dimension point'
         , Point_ point  (Dimension point) (NumType point)
         , Point_ point' (Dimension point) (NumType point')
         ) => HasPoints (LineSegment endPoint point) (LineSegment endPoint point')
                        point                        point' where
  allPoints f (LineSegment s t) = liftF2 LineSegment (traverse1 f s) (traverse1 f t)


-- foo     :: ( Eq r
--          , Additive_ (VectorFamily' d r)
--          , IxValue (VectorFamily' d r) ~ r
--          , HasComponents (VectorFamily' d Bool) Bool
--          ) => Vector d r -> Vector d r -> Bool




instance ( Traversable1 endPoint
         , Point_ point d r
         , d ~ Dimension point, r ~  NumType point
         , Ord r
         , Ord (Vector d r)
         ) => IsBoxable (LineSegment endPoint point)


-- deriving instance (Show (Interval endPoint point)) => Show (LineSegment endPoint point)

instance {-# OVERLAPPABLE #-}
         ( Show (endPoint point)
         -- , OptCVector_ 2 (endPoint point)
         ) => Show (LineSegment endPoint point) where
  showsPrec k (LineSegment s t) = showParen (k > appPrec) $
                                    showString "LineSegment "
                                    . showsPrec (appPrec+1) s
                                    . showChar ' '
                                    . showsPrec (appPrec+1) t

instance {-# OVERLAPPING #-}
         ( Show point
         -- , OptCVector_ 2 point
         ) => Show (ClosedLineSegment point) where
  showsPrec k (ClosedLineSegment s t) = showParen (k > appPrec) $
                                    showString "ClosedLineSegment "
                                    . showsPrec (appPrec+1) s
                                    . showChar ' '
                                    . showsPrec (appPrec+1) t

appPrec :: Int
appPrec = 10

instance ( Read (endPoint point) --, OptCVector_ 2 (endPoint point))
         ) => Read (LineSegment endPoint point) where
  readPrec = parens (prec appPrec $ do
                          Ident "LineSegment" <- lexP
                          p <- step readPrec
                          q <- step readPrec
                          return (LineSegment p q))

instance {-# OVERLAPPING #-}
         (Read point -- , OptCVector_ 2 point)
         ) => Read (ClosedLineSegment point) where
  readPrec = parens (prec appPrec $ do
                          Ident "ClosedLineSegment" <- lexP
                          p <- step readPrec
                          q <- step readPrec
                          return (ClosedLineSegment p q))


-- type instance VectorFamily d (LineSegment endPoint point) =
--   WrapVector d (Interval endPoint point) (LineSegment endPoint point)



instance ( Point_ point d r
         , Has_ Metric_ d r
         , EndPoint_ (endPoint point)
         , IxValue (endPoint point) ~ point
         , Num r
         ) => HasSupportingLine (LineSegment endPoint point) where
  supportingLine seg = lineThrough @(LinePV d r) @point
                                   (seg^.start) (seg^.end)
  {-# INLINE supportingLine #-}

instance ( Fractional r, Ord r
         , HasSquaredEuclideanDistance point
         , Point_ point d r
         ) => HasSquaredEuclideanDistance (ClosedLineSegment point) where
  pointClosestToWithDistance q seg@(ClosedLineSegment a b)
      | m `intersects` seg = z
      | otherwise          = minOn snd (pointClosestToWithDistance q a)
                                       (pointClosestToWithDistance q b)
    where
      z       :: (Point d r, r)
      z@(m,_) = pointClosestToWithDistance q (supportingLine seg)

      minOn       :: Ord b => (a -> b) -> a -> a -> a
      minOn f x y = if f x <= f y then x else y

-- type Intersection (Point d r) (ClosedLineSegment point)

instance ( OnSegment (LineSegment endPoint point)
         , Point_ point d r
         , Fractional r, Ord r
         -- , OptVector_ d r, OptMetric_ d r
         ) => Point d r `HasIntersectionWith` LineSegment endPoint point where
  -- >>> test `intersects` testseg
  -- True
  intersects = onSegment

instance ( -- OptCVector_ 2 point
         -- ,
           Point_ point d r
         , Fractional r
         ) => OnSegment (ClosedLineSegment point) where
  onSegment (view asPoint -> q) seg =
    case (q .-. (seg^.start.asPoint)) `scalarMultiple` ((seg^.end) .-. (seg^.start)) of
      Nothing     -> False
      Just lambda -> 0 <= lambda && lambda <= 1

instance ( -- OptCVector_ 2 point
         -- ,
           Point_ point d r
         , Fractional r
         ) => OnSegment (OpenLineSegment point) where
  onSegment (view asPoint -> q) seg =
    case (q .-. (seg^.start.asPoint)) `scalarMultiple` ((seg^.end) .-. (seg^.start)) of
      Nothing     -> False
      Just lambda -> 0 < lambda && lambda < 1

instance ( -- OptCVector_ 2 (AnEndPoint point)
         -- ,
           Point_ point d r
         , Fractional r
         ) => OnSegment (LineSegment AnEndPoint point) where
  onSegment (view asPoint -> q) seg =
      case (q .-. (seg^.start.asPoint)) `scalarMultiple` ((seg^.end) .-. (seg^.start)) of
        Nothing     -> False
        Just lambda -> compare' (seg^.startPoint.to endPointType) 0      lambda
                    && compare' (seg^.endPoint.to endPointType)   lambda 1
    where
      compare' = \case
        Open   -> (<)
        Closed -> (<=)

-- FIXME: In R^2 we can give an implementation that just uses Num


-- instance ( DefaultTransformByConstraints (LineSegment endPoint point) d r
--          , Point_ point d r
--          , d > 0
--          ) => IsTransformable (LineSegment endPoint point)





  -- traverse1' f xs =
  --     -- Get the length of the vector in /O(1)/ time
  --     let !n = G.length xs
  --     -- Use fromListN to be more efficient in construction of resulting vector
  --     -- Also behaves better with compact regions, preventing runtime exceptions
  --     in  Data.Vector.fromListN n Applicative.<$> Traversable.traverse f (toList xs<)


-- type R = RealNumber 5

-- intersects2 :: Point 2 R -> ClosedLineSegment (Point 2 R) -> Bool
-- intersects2 = intersects

-- testseg1 :: ClosedLineSegment (Point 2 R)
-- testseg1 = ClosedLineSegment (Point2 5.0 6.0) (Point2 10.0 10.0)

-- myPoint :: Point 2 R
-- myPoint = Point2 11 11
