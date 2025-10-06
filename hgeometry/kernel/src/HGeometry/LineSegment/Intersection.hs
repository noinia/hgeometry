{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.LineSegment.Intersection
  ( LineLineSegmentIntersection(..)
  , LineSegmentLineSegmentIntersection(..)
  , HalfLineLineSegmentIntersection(..)
  -- , spansIntersect
  , compareColinearInterval

  , ClosedSegmentHalfSpaceIntersection(..)
  ) where

import Control.Lens
import GHC.Generics (Generic)
import HGeometry.Box.Intersection ()
import HGeometry.Ext
import HGeometry.HalfLine
import HGeometry.HalfSpace
import HGeometry.HyperPlane.Class
import HGeometry.Intersection
import HGeometry.Interval
import HGeometry.Line
import HGeometry.LineSegment.Internal
import HGeometry.Point
import HGeometry.Point.Either
import HGeometry.Properties (NumType, Dimension)

--------------------------------------------------------------------------------
-- * Line x LineSegment Intersection

-- | Line x LineSegment intersection
data LineLineSegmentIntersection lineSegment =
    Line_x_LineSegment_Point       (Point 2 (NumType lineSegment))
  | Line_x_LineSegment_LineSegment lineSegment

deriving instance (Show (Point 2 (NumType lineSegment)), Show lineSegment
                  ) => Show (LineLineSegmentIntersection lineSegment)
deriving instance (Eq (Point 2 (NumType lineSegment)), Eq lineSegment
                  ) => Eq (LineLineSegmentIntersection lineSegment)

type Intersection' lineSegment = Maybe (LineLineSegmentIntersection lineSegment)

type instance Intersection (LinePV 2 r) (LineSegment (EndPoint t) point) =
  Intersection' (LineSegment (EndPoint t) point)

type instance Intersection (LineEQ r) (LineSegment (EndPoint t) point) =
  Intersection' (LineSegment (EndPoint t) point)

type instance Intersection (LinePV 2 r) (LineSegment AnEndPoint point) =
  Intersection' (LineSegment AnEndPoint point)

type instance Intersection (LineEQ r) (LineSegment AnEndPoint point) =
  Intersection' (LineSegment AnEndPoint point)

----------------------------------------
-- * HasIntersectionWith

instance ( Point_ point 2 r, Num r, Ord r
         , IxValue (endPoint point) ~ point
         , EndPoint_ (endPoint point)
         ) => LinePV 2 r `HasIntersectionWith` LineSegment endPoint point where
  intersects = intersectsImpl
  {-# INLINE intersects #-}

instance ( Point_ point 2 r, Num r, Ord r
         , IxValue (endPoint point) ~ point
         , EndPoint_ (endPoint point)
         ) => LineEQ r `HasIntersectionWith` LineSegment endPoint point where
  --
  -- >>> LineEQ 1 2 `intersects` ClosedLineSegment origin (Point2 1 10)
  -- True
  intersects = intersectsImpl
  {-# INLINE intersects #-}

-- | Test whether a line in R^2 intersects a closed linesegment
intersectsImpl       :: ( HyperPlane_ line 2 r
                        , Point_ point 2 r
                        , Num r, Ord r
                        , LineSegment_ lineSegment point
                        ) => line -> lineSegment -> Bool
l `intersectsImpl` s = case (onSideTest (s^.start) l, onSideTest (s^.end) l) of
                         (LT, LT) -> False
                         (LT, EQ) -> s^.endPoint.to endPointType == Closed
                         (LT, GT) -> True
                         (EQ, EQ) -> True
                         (EQ, _)  -> s^.startPoint.to endPointType == Closed
                         (GT, LT) -> True
                         (GT, EQ) -> s^.endPoint.to endPointType == Closed
                         (GT, GT) -> False
{-# INLINE intersectsImpl #-}


----------------------------------------
-- * IsIntersectableWith

instance ( Point_ point 2 r
         , Fractional r,  Ord r
         ) => LinePV 2 r `IsIntersectableWith` ClosedLineSegment point where
  intersect = intersectImpl
  {-# INLINE intersect #-}

instance ( Point_ point 2 r
         , Fractional r,  Ord r
         ) => LineEQ r `IsIntersectableWith` ClosedLineSegment point where
  intersect = intersectImpl
  {-# INLINE intersect #-}

instance ( Point_ point 2 r
         , Fractional r,  Ord r
         ) => LinePV 2 r `IsIntersectableWith` LineSegment AnEndPoint point where
  intersect = intersectImpl
  {-# INLINE intersect #-}

instance ( Point_ point 2 r
         , Fractional r,  Ord r
         ) => LineEQ r `IsIntersectableWith` LineSegment AnEndPoint point where
  intersect = intersectImpl
  {-# INLINE intersect #-}

instance ( Point_ point 2 r
         , Fractional r,  Ord r
         ) => LinePV 2 r `IsIntersectableWith` OpenLineSegment point where
  intersect = intersectImpl
  {-# INLINE intersect #-}

instance ( Point_ point 2 r
         , Fractional r,  Ord r
         ) => LineEQ r `IsIntersectableWith` OpenLineSegment point where
  intersect = intersectImpl
  {-# INLINE intersect #-}


-- | Implementation for intersects between lines and line segments.
--
-- the type is is sufficiently general that for various line or closed line segment types
-- we can appeal to it.
intersectImpl       :: ( HyperPlane_ line 2 r
                       , Point_ point 2 r
                       , Fractional r, Ord r
                       , LineSegment_ lineSegment point
                       , HasSupportingLine lineSegment
                       , HasOnSegment lineSegment 2
                       , line `IsIntersectableWith` LinePV 2 r
                       , line `HasIntersectionWith` lineSegment
                       , Intersection line (LinePV 2 r) ~ Maybe (LineLineIntersection line)
                       ) => line -> lineSegment
                    -> Maybe (LineLineSegmentIntersection lineSegment)
l `intersectImpl` s = l `intersect` supportingLine s >>= \case
    Line_x_Line_Point p | p `onSegment` s -> Just $ Line_x_LineSegment_Point p
                        | otherwise       -> Nothing
    Line_x_Line_Line _  -> Just $ Line_x_LineSegment_LineSegment s
{-# INLINE intersectImpl #-}


--------------------------------------------------------------------------------
-- * LineSegment x LineSegment

-- | LineSegment x LineSegment intersection
data LineSegmentLineSegmentIntersection lineSegment =
    LineSegment_x_LineSegment_Point       (Point 2 (NumType lineSegment))
  | LineSegment_x_LineSegment_LineSegment lineSegment

deriving instance (Show (Point 2 (NumType lineSegment)), Show lineSegment
                  ) => Show (LineSegmentLineSegmentIntersection lineSegment)
deriving instance (Eq (Point 2 (NumType lineSegment)), Eq lineSegment
                  ) => Eq (LineSegmentLineSegmentIntersection lineSegment)

type instance Intersection (LineSegment (EndPoint t) point)
                           (LineSegment (EndPoint t) point) =
  Maybe (LineSegmentLineSegmentIntersection (LineSegment (EndPoint t) point))

type instance Intersection (LineSegment AnEndPoint point)
                           (LineSegment AnEndPoint point) =
  Maybe (LineSegmentLineSegmentIntersection (LineSegment AnEndPoint point))


-- | fmap' for lineSegmentIntersections
fmap'   :: (NumType lineSegment ~ NumType lineSegment')
        => (lineSegment -> lineSegment')
        -> LineSegmentLineSegmentIntersection lineSegment
        -> LineSegmentLineSegmentIntersection lineSegment'
fmap' f = \case
  LineSegment_x_LineSegment_Point p       -> LineSegment_x_LineSegment_Point p
  LineSegment_x_LineSegment_LineSegment s -> LineSegment_x_LineSegment_LineSegment (f s)

----------------------------------------
-- * HasIntersectionWith

-- | Test if the spans (i.e. the projections onto the x-axis and on the yaxis) of the
-- segments intersect
spansIntersect      :: forall endPoint endPoint' point point' r.
                       ( Point_ point 2 r, Point_ point' 2 r
                       , Ord r, Num r, Functor endPoint, Functor endPoint'
                       , IxValue (endPoint point) ~ point
                       , EndPoint_ (endPoint point)
                       , IxValue (endPoint' point') ~ point'
                       , EndPoint_ (endPoint' point')
                       , HasIntersectionWith (Interval endPoint r) (Interval endPoint' r)
                       )
                    => LineSegment endPoint point -> LineSegment endPoint' point' -> Bool
spansIntersect s s' = f xCoord && f yCoord
  where
    f         :: (forall aPoint. Point_ aPoint 2 r => Getter aPoint r) -> Bool
    f coord'' = spanIn coord'' s `intersects` spanIn coord'' s'

instance ( Point_ point 2 r, Point_ point' 2 r, Num r,  Ord r
         , Functor endPoint
         , IxValue (endPoint point) ~ point, EndPoint_ (endPoint point)
         , IxValue (endPoint point') ~ point', EndPoint_ (endPoint point')
         , IxValue (endPoint r) ~ r, EndPoint_ (endPoint r)
         , HasIntersectionWith (Interval endPoint r) (Interval endPoint r)
         ) =>
         LineSegment endPoint point `HasIntersectionWith` LineSegment endPoint point' where
  s `intersects `s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
                      && spansIntersect s s'
  {-# INLINE intersects #-}

instance ( Point_ point 2 r, Point_ point' 2 r, Num r,  Ord r
         ) =>
         LineSegment AnEndPoint point `HasIntersectionWith` ClosedLineSegment point' where
  s `intersects `s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
                      && spansIntersect s s'
  {-# INLINE intersects #-}

instance ( Point_ point 2 r, Point_ point' 2 r, Num r,  Ord r
         ) =>
         LineSegment AnEndPoint point `HasIntersectionWith` OpenLineSegment point' where
  s `intersects `s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
                      && spansIntersect s s'
  {-# INLINE intersects #-}

instance ( Point_ point 2 r, Point_ point' 2 r, Num r,  Ord r
         ) =>
         ClosedLineSegment point `HasIntersectionWith` LineSegment AnEndPoint point' where
  s `intersects `s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
                      && spansIntersect s s'
  {-# INLINE intersects #-}

instance ( Point_ point 2 r, Point_ point' 2 r, Num r,  Ord r
         ) =>
         ClosedLineSegment point `HasIntersectionWith` OpenLineSegment point' where
  s `intersects `s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
                      && spansIntersect s s'
  {-# INLINE intersects #-}

instance ( Point_ point 2 r, Point_ point' 2 r, Num r,  Ord r
         ) =>
         OpenLineSegment point `HasIntersectionWith` LineSegment AnEndPoint point' where
  s `intersects `s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
                      && spansIntersect s s'
  {-# INLINE intersects #-}

instance ( Point_ point 2 r, Point_ point' 2 r, Num r,  Ord r
         ) =>
         OpenLineSegment point `HasIntersectionWith` ClosedLineSegment point' where
  s `intersects `s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
                      && spansIntersect s s'
  {-# INLINE intersects #-}

----------------------------------------
-- * IsIntersectableWith

-- instance ( Point_ point 2 r, Fractional r,  Ord r
--          ) =>
--          LineSegment AnEndPoint point `IsIntersectableWith` LineSegment AnEndPoint point where
--   s `intersect` s' = supportingLine s `intersect` s' >>= \case
--     Line_x_LineSegment_Point p
--       | p `onSegment` s              -> Just $ LineSegment_x_LineSegment_Point p
--       | otherwise                    -> Nothing
--     Line_x_LineSegment_LineSegment _ -> Just $ LineSegment_x_LineSegment_LineSegment todo
--       where
--         todo = error "LineSegment_x_LineSegment_LineSegment, not yet implemented"
--   {-# INLINE intersect #-}

instance ( Point_ point 2 r, Num r,  Ord r
         , Functor endPoint
         , IxValue (endPoint point) ~ point, EndPoint_ (endPoint point)
         , IxValue (endPoint r) ~ r, EndPoint_ (endPoint r)
         , HasIntersectionWith (Interval endPoint r) (Interval endPoint r)
         , IxValue (endPoint point) ~ point
         , IxValue (endPoint (r :+ endPoint point)) ~ (r :+ endPoint point)
         , EndPoint_ (endPoint point)
         , IsEndPoint (endPoint point) (endPoint (r :+ endPoint point))
         , IsIntersectableWith (LinePV 2 r) (LineSegment endPoint point)
         , Intersection (LinePV 2 r) (LineSegment endPoint point)
           ~ Maybe (LineLineSegmentIntersection (LineSegment endPoint point))
         , HasOnSegment (LineSegment endPoint point) 2
         , IsIntersectableWith (Interval endPoint (r :+ endPoint point))
                               (Interval endPoint (r :+ endPoint point))
         , Intersection (LineSegment endPoint point) (LineSegment endPoint point)
           ~ Maybe (LineSegmentLineSegmentIntersection (LineSegment endPoint point))
         , EndPoint_ (endPoint (r :+ endPoint point))
         ) => LineSegment endPoint point `IsIntersectableWith` LineSegment endPoint point where
  s `intersect` s' = supportingLine s `intersect` s' >>= \case
      Line_x_LineSegment_Point p
        | p `onSegment` s              -> Just $ LineSegment_x_LineSegment_Point p
        | otherwise                    -> Nothing
      Line_x_LineSegment_LineSegment _ -> spanIn' s `intersect` spanIn' s' <&> \case
        Interval_x_Interval_Point xy   -> LineSegment_x_LineSegment_Point $
                                            xy^.extra._endPoint.asPoint
        Interval_x_Interval_Contained i -> mkIntersect i
        Interval_x_Interval_Partial i   -> mkIntersect i
    where
      mkIntersect i =
        LineSegment_x_LineSegment_LineSegment $ LineSegment (i^.start.extra) (i^.end.extra)

-- | Given a line segment, compute the span of the line segment. In principle we compute
-- the span in terms of the x-coordinate. Except wehn the segment is vertical, then we return
-- the span in the y-cooridnate instead.
spanIn'  :: ( Point_ point 2 r, Ord r
            , IxValue (endPoint point) ~ point
            , IxValue (endPoint (r :+ endPoint point)) ~ (r :+ endPoint point)
            , EndPoint_ (endPoint point)
            , IsEndPoint (endPoint point) (endPoint (r :+ endPoint point))
            ) => LineSegment endPoint point -> Interval endPoint (r :+ endPoint point)
spanIn' seg@(LineSegment s t) = case (seg^.start.xCoord) `compare` (seg^.end.xCoord) of
    LT                                        -> Interval (xLabel s) (xLabel t)
    EQ | seg^.start.yCoord <= seg^.end.yCoord -> Interval (yLabel s) (yLabel t)
       | otherwise                            -> Interval (yLabel t) (yLabel s)
    GT                                        -> Interval (xLabel t) (xLabel s)
  where
    xLabel p = p&_endPoint %~ \pt -> pt^.xCoord :+ p
    yLabel p = p&_endPoint %~ \pt -> pt^.yCoord :+ p

-- data XY = X | Y deriving (Show,Eq)


{-

instance ( Point_ point 2 r, Num r, Ord r
         ) => ClosedLineSegment point `HasIntersectionWith` ClosedLineSegment point where
  sa `intersects` sb = supportingLine sa `intersects` sb &&
                       supportingLine sb `intersects` sa
  {-# INLINE intersects #-}
  -- FIXME: this is not correct yet; i.e. if sa and sb are colinear both supportinging
  -- lines intersect the segment, but the segments may stil lbe disjoint.


instance ( Point_ point 2 r
         , Fractional r,  Ord r
         ) => ClosedLineSegment point `IsIntersectableWith` ClosedLineSegment point where
  sa `intersect` sb
    | sa `intersects` sb = Just undefined
      -- FIXME!! continue here
    | otherwise          = Nothing
  {-# INLINE intersect #-}

-}



--------------------------------------------------------------------------------

type instance Intersection (LineSegment endPoint point :+ extra)
                           (LineSegment endPoint point :+ extra) =
  Maybe (LineSegmentLineSegmentIntersection (LineSegment endPoint point :+ extra))
  -- FIXME: hmm, this type is kind of nonsense, since the intersecting segment may be from
  -- different segments

-- instance ( LineSegment endPoint point `HasIntersectionWith` LineSegment endPoint point
--          ) => (LineSegment endPoint point :+ extra) `HasIntersectionWith`
--               (LineSegment endPoint point :+ extra') where
--   (s :+ _) `intersects` (s' :+ _) = s `intersects` s'

instance ( LineSegment endPoint point `IsIntersectableWith` LineSegment endPoint point
         , Intersection (LineSegment endPoint point) (LineSegment endPoint point)
           ~ Maybe (LineSegmentLineSegmentIntersection (LineSegment endPoint point))
         ) => (LineSegment endPoint point :+ extra) `IsIntersectableWith`
              (LineSegment endPoint point :+ extra) where
  (s :+ _) `intersect` (s' :+ _) = fmap' (:+ undef') <$> s `intersect` s'
    where
      undef' = error "intersect semgnets: not possible"



--------------------------------------------------------------------------------
-- * Intersection with HalfLines

instance ( Ord r, Num r, Point_ point 2 r
         , IxValue (endPoint point) ~ point, EndPoint_ (endPoint point)
         ) => HasIntersectionWith (HalfLine point) (LineSegment endPoint point) where
  hl `intersects` seg = supportingLine hl `intersects` seg && supportingLine seg `intersects` hl
  {-# INLINE intersects #-}
  -- this is incorrect! in particular if hl and seg lie on the same line


type instance Intersection (HalfLine point) (LineSegment endPoint point)
  = Maybe (HalfLineLineSegmentIntersection (Point 2 (NumType point))
                                           (LineSegment endPoint point))

-- | Data type representing the intersection of a HalfLine and a LineSegment
data HalfLineLineSegmentIntersection point segment =
      HalfLine_x_LineSegment_Point       point
    | HalfLine_x_LineSegment_LineSegment segment
  deriving (Show,Eq,Read,Ord,Generic,Functor)

instance ( Ord r, Fractional r, Point_ point 2 r
         , IxValue (endPoint point) ~ point
         , EndPoint_ (endPoint point)
         , IsIntersectableWith (LinePV 2 r) (LineSegment endPoint point)
         , Intersection (LinePV 2 r) (LineSegment endPoint point)
           ~ Maybe (LineLineSegmentIntersection lineSegment')
         , NumType lineSegment' ~ r
         ) => IsIntersectableWith (HalfLine point) (LineSegment endPoint point) where
  hl `intersect` seg = m `intersect` seg >>= \case
      Line_x_LineSegment_Point q
        | q `onSide` perpendicularTo m == LeftSide -> Just $ HalfLine_x_LineSegment_Point q
        | otherwise                                -> Nothing
      Line_x_LineSegment_LineSegment _             -> case compareColinearInterval m seg of
        Before   -> Just $ HalfLine_x_LineSegment_LineSegment seg
        OnStart  -> Just $ HalfLine_x_LineSegment_LineSegment seg
        Interior -> Just $ HalfLine_x_LineSegment_LineSegment $ seg&start .~ (hl^.start)
        OnEnd
          | isClosed (seg^.endPoint) -> Just $ HalfLine_x_LineSegment_Point (seg^.end.asPoint)
          | otherwise                -> Nothing
        After                        -> Nothing -- no intersection
    where
      m = supportingLine hl
      isClosed = (== Closed) . endPointType

-- | Given a line l, and a line segment seg that lies on l. Returns where the anchorPoint
-- of the line is with respect to the line segment (which we can interpret as some
-- interval along l).
compareColinearInterval                    :: ( Ord r, Num r
                                              , Point_ point 2 r
                                              , IxValue (endPoint point) ~ point
                                              , EndPoint_ (endPoint point)
                                              )
                                           => LinePV 2 r -> LineSegment endPoint point
                                           -> CompareInterval
compareColinearInterval l@(LinePV p _) seg = case p `onSide` mStart of
    RightSide -> Before
    OnLine    -> OnStart
    LeftSide  -> case p `onSide` mEnd of
      RightSide -> Interior
      OnLine    -> OnEnd
      LeftSide  -> After
  where
    m = perpendicularTo l
    mStart = m&anchorPoint .~ seg^.start.asPoint
    mEnd   = m&anchorPoint .~ seg^.end.asPoint
    -- the left side is the side in which the vector v points.

type instance NumType   (HalfLineLineSegmentIntersection point edge) = NumType point
type instance Dimension (HalfLineLineSegmentIntersection point edge) = Dimension point

instance ( HasSquaredEuclideanDistance point
         , HasSquaredEuclideanDistance segment
         , NumType point ~ NumType segment, Dimension point ~ Dimension segment
         ) => HasSquaredEuclideanDistance (HalfLineLineSegmentIntersection point segment) where
  pointClosestToWithDistance q = \case
    HalfLine_x_LineSegment_Point p         -> pointClosestToWithDistance q p
    HalfLine_x_LineSegment_LineSegment seg -> pointClosestToWithDistance q seg


--------------------------------------------------------------------------------
-- * Intersection of HalfSpaces and Line segments

instance ( Point_ point d r, Ord r, Num r
         , HyperPlane_ plane d r
         ) => HasIntersectionWith (ClosedLineSegment point) (HalfSpaceF plane) where
  seg `intersects` halfSpace = (seg^.start.asPoint) `intersects` halfSpace
                            || (seg^.end.asPoint)   `intersects` halfSpace


type instance Intersection (ClosedLineSegment point) (HalfSpaceF plane) =
  Maybe (ClosedSegmentHalfSpaceIntersection (CanonicalPoint point)
                                            point
        )


-- | Models the intersection of a closed linesegment and halfspace
-- if the segment intersects the bounding hyperplane, the intersection point
-- is of type extra.
data ClosedSegmentHalfSpaceIntersection extra point =
    ClosedLineSegment_x_HalfSpace_Point           point
  | ClosedLineSegment_x_HalfSpace_SubSegment      (ClosedLineSegment (OriginalOrExtra point extra))
  -- ^ the subsegment is always oriented from the intersection point towards the original point
  -- note that this may reverse the original input segment.
  | ClosedLineSegment_x_HalfSpace_CompleteSegment (ClosedLineSegment point)
  deriving (Show,Eq)


instance ( Point_ point 2 r, Ord r, Fractional r
         , HyperPlane_ plane 2 r
         , IsIntersectableWith (LinePV 2 r) plane
         , Intersection (LinePV 2 r) plane ~ Maybe (LineLineIntersectionG r line')
         ) => IsIntersectableWith (ClosedLineSegment point) (HalfSpaceF plane) where
  seg `intersect` halfSpace = case (seg^.start.asPoint) `intersect` halfSpace of
      Nothing -> case mxEnd of
        Nothing   -> Nothing
        Just xEnd -> Just $ case xEnd of
          Point_x_HalfSpace_OnBoundary _ -> ClosedLineSegment_x_HalfSpace_Point (seg^.end)
          Point_x_HalfSpace_Interior   t -> subSegment (seg^.end) t (seg^.start.asPoint)

      Just xStart -> Just $ case xStart of
        Point_x_HalfSpace_OnBoundary _ -> case mxEnd of
          Nothing  -> ClosedLineSegment_x_HalfSpace_Point (seg^.start)
          Just _   -> completeSeg
        Point_x_HalfSpace_Interior s   -> case mxEnd of
          Nothing  -> subSegment (seg^.start) s (seg^.end.asPoint)
          Just _   -> completeSeg
    where
      completeSeg = ClosedLineSegment_x_HalfSpace_CompleteSegment seg
      mxEnd = (seg^.end.asPoint)   `intersect` halfSpace

      -- Compute the subsegment ; we are guarnteed that inP is inside and outP is outside
      -- the haflspace
      subSegment inP' inP outP = ClosedLineSegment_x_HalfSpace_SubSegment
                               . ClosedLineSegment (Original inP')
                               $ case LinePV inP (outP .-. inP)
                                       `intersect` (halfSpace^.boundingHyperPlane) of
        Just (Line_x_Line_Point p) -> Extra p
        _                          -> error "line segment x halfspace intersection: absurd"
