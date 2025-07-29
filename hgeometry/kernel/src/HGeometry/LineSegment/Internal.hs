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
  , spanIn
  , EndPoint_(..)
  , asALineSegment
  ) where


import Control.Lens
import Data.Functor.Apply
import Data.Kind (Type)
import HGeometry.Box.Boxable
import HGeometry.Intersection
import HGeometry.Interval
import HGeometry.Line.Class
import HGeometry.Line.PointAndVector
import HGeometry.LineSegment.Class
import HGeometry.Point
import HGeometry.Properties (NumType, Dimension)
import HGeometry.Transformation
import HGeometry.Vector
import Text.Read

-- import HGeometry.Number.Real.Rational
-- import Debug.Trace

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

-- | A type representing Closed LineSegments
type ClosedLineSegment point = LineSegment (EndPoint Closed) point

-- | A type representing Open LineSegments
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

deriving instance (Eq (endPoint point)
                  ) => Eq (LineSegment endPoint point)
deriving instance (Ord (endPoint point)
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
         , EndPoint_ (endPoint point)
         ) => HasStart (LineSegment endPoint point) point where
  start = _LineSegmentInterval.start

instance ( IxValue (endPoint point) ~ point
         ) => HasStartPoint (LineSegment endPoint point) (endPoint point) where
  startPoint = _LineSegmentInterval.startPoint


instance ( IxValue (endPoint point) ~ point
         , EndPoint_ (endPoint point)
         ) => HasEnd (LineSegment endPoint point) point where
  end = _LineSegmentInterval.end

instance ( IxValue (endPoint point) ~ point
         ) => HasEndPoint (LineSegment endPoint point) (endPoint point) where
  endPoint = _LineSegmentInterval.endPoint

type instance StartPointOf (LineSegment endPoint point) = endPoint point
type instance EndPointOf   (LineSegment endPoint point) = endPoint point

instance ( IxValue (endPoint point) ~ point
         , EndPoint_ (endPoint point)
         ) => IntervalLike_ (LineSegment endPoint point) point where

instance ( IxValue (endPoint point) ~ point
         , EndPoint_ (endPoint point)
         , Point_ point (Dimension point) (NumType point)
         ) => LineSegment_ (LineSegment endPoint point) point where

instance ( IxValue (endPoint point) ~ point
         , EndPoint_ (endPoint point)
         , Point_ point (Dimension point) (NumType point)
         ) => ConstructableLineSegment_ (LineSegment endPoint point) point where
  uncheckedLineSegment s t = LineSegment (mkEndPoint s) (mkEndPoint t)

instance ( Point_ point (Dimension point) (NumType point)
         ) => ClosedLineSegment_ (ClosedLineSegment point) point where

instance ( Point_ point (Dimension point) (NumType point)
         ) => OpenLineSegment_ (OpenLineSegment point) point where

instance ( Traversable1 endPoint
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

instance ( HasOnSegment (LineSegment endPoint point) d
         , Point_ point d r
         , Fractional r, Ord r
         ) => Point d r `HasIntersectionWith` LineSegment endPoint point where
  -- >>> test `intersects` testseg
  -- True
  intersects = onSegment

instance {-# OVERLAPPING #-}
         ( HasOnSegment (LineSegment endPoint point) 2
         , Point_ point 2 r
         , Num r, Ord r
         , IxValue (endPoint point) ~ point, EndPoint_ (endPoint point)
         ) => Point 2 r `HasIntersectionWith` LineSegment endPoint point where
  -- >>> test `intersects` testseg
  -- True
  intersects = onSegment2

-- | Implementation of OnSegment for 2 dimensional segments that only uses Ord and Num r
-- constraints.
--
onSegment2                          :: ( Point_ point 2 r, Point_ point' 2 r, Ord r, Num r
                                       , LineSegment_ lineSegment point')
                                    => point -> lineSegment -> Bool
onSegment2 q seg@(LineSegment_ s t) =
    onLine q supLine && shouldBe (seg^.startPoint.to endPointType) (onSide q l) LeftSide
                     && shouldBe (seg^.endPoint.to endPointType)   (onSide q r) RightSide
  where
    supLine = LinePV (s^.asPoint) (t .-. s)
    l = perpendicularTo supLine
    r = perpendicularTo $ LinePV (t^.asPoint) (t .-. s)

    -- shouldBe t a side is essentially a == side, but in case the type is open
    -- we alos allow to be on the line.
    shouldBe et a side = case et of
      Open   -> a == side
      Closed -> a == side || a == OnLine

instance {-# OVERLAPPING #-} ( Point_ point 2 r, Num r
         ) => HasOnSegment (ClosedLineSegment point) 2 where
  onSegment = onSegment2
instance {-# OVERLAPPING #-} ( Point_ point 2 r, Num r
         ) => HasOnSegment (OpenLineSegment point) 2 where
  onSegment = onSegment2
instance {-# OVERLAPPING #-} ( Point_ point 2 r, Num r
         ) => HasOnSegment (LineSegment AnEndPoint point) 2 where
  onSegment = onSegment2

instance ( Point_ point d r, Fractional r) => HasOnSegment (ClosedLineSegment point) d where
  onSegment = onClosedSegmentD

instance ( Point_ point d r, Fractional r) => HasOnSegment (OpenLineSegment point) d where
  onSegment = onOpenSegmentD

instance ( Point_ point d r, Fractional r) => HasOnSegment (LineSegment AnEndPoint point) d where
  onSegment = onSegmentD

-- | Implementation of onSegment for d-dimensional segments. The function should
onSegmentDWith                           :: ( Point_ point d r
                                            , Point_ point' d r
                                            , LineSegment_ lineSegment point'
                                            , Fractional r, Eq r
                                            )
                                         => (r -> Bool)
                                         -> point -> lineSegment -> Bool
onSegmentDWith f (view asPoint -> q) seg = maybe False f mLambda
  where
    mLambda = (q .-. (seg^.start.asPoint)) `scalarMultiple` ((seg^.end) .-. (seg^.start))

onClosedSegmentD :: ( Point_ point d r, Point_ point' d r, Fractional r, Ord r
                    , ClosedLineSegment_ lineSegment point'
                    ) => point -> lineSegment -> Bool
onClosedSegmentD = onSegmentDWith $ \lambda -> 0 <= lambda && lambda <= 1

onOpenSegmentD :: ( Point_ point d r, Point_ point' d r, Fractional r, Ord r
                  , OpenLineSegment_ lineSegment point'
                  ) => point -> lineSegment -> Bool
onOpenSegmentD = onSegmentDWith $ \lambda -> 0 < lambda && lambda < 1

onSegmentD      :: ( Point_ point d r, Point_ point' d r, Fractional r, Ord r
                   ) => point -> LineSegment AnEndPoint point' -> Bool
onSegmentD q seg = onSegmentDWith f q seg
  where
    f lambda = compare' (seg^.startPoint.to endPointType) 0      lambda
            && compare' (seg^.endPoint.to endPointType)   lambda 1
    compare' = \case
      Open   -> (<)
      Closed -> (<=)


instance ( Point_ point d r
         , IxValue (endPoint point) ~ point
         , EndPoint_ (endPoint point)
         , IsTransformable point
         ) => IsTransformable (LineSegment endPoint point) where
  transformBy t s = s&start %~ transformBy t
                     &end   %~ transformBy t


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


--------------------------------------------------------------------------------

-- | Computes the span of the interval in the given direction. Note that the returned
-- interval is a proper interval, i.e. with the start smaller than the end.
--
-- >>> spanIn xCoord (ClosedLineSegment (Point2 5 (10 :: Int)) (Point2 20 0))
-- Interval (AnEndPoint Closed 5) (AnEndPoint Closed 20)
-- >>> spanIn yCoord (ClosedLineSegment (Point2 5 (10 :: Int)) (Point2 20 0))
-- Interval (AnEndPoint Closed 0) (AnEndPoint Closed 10)
spanIn             :: ( Point_ point d r, Ord r
                      , IxValue (endPoint point) ~ point
                      -- , EndPoint_ (endPoint r)
                      , EndPoint_ (endPoint point)
                      )
                   => Getter point r
                   -> LineSegment endPoint point
                   -> Interval AnEndPoint r
spanIn coord'' seg = case (i^.start) `compare` (i^.end) of
                       LT -> i
                       EQ -> Interval (AnClosedE (s^._endPoint)) (AnClosedE (e^._endPoint))
                       GT -> Interval e s
  where
    i@(Interval s e) = view coord'' <$> seg^._LineSegmentInterval.to asAnInterval


-- | convert into an LineSegment whose endpoints are explicitly tagged.
asALineSegment   :: LineSegment_ segment point => segment -> LineSegment AnEndPoint point
asALineSegment e = LineSegment (asAnEndPoint $ e^.startPoint) (asAnEndPoint $ e^.endPoint)
