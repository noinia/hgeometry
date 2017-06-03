{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.LineSegment( LineSegment
                                , pattern LineSegment
                                , pattern LineSegment'
                                , pattern ClosedLineSegment

                                , _SubLine
                                , module Data.Geometry.Interval


                                , toLineSegment
                                , onSegment
                                , orderedEndPoints
                                , segmentLength
                                , sqDistanceToSeg, sqDistanceToSegArg
                                , flipSegment
                                ) where

import           Data.Ord(comparing)
import           Control.Arrow((&&&))
import           Control.Lens
import           Data.Bifunctor
import           Data.Semigroup
import           Data.Ext
import           Data.Geometry.Box.Internal
import           Data.Geometry.Interval
import           Data.Geometry.Line.Internal
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.SubLine
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import           Data.Vinyl
import           Data.UnBounded
import           Frames.CoRec
import qualified Data.Foldable as F

--------------------------------------------------------------------------------
-- * d-dimensional LineSegments


-- | Line segments. LineSegments have a start and end point, both of which may
-- contain additional data of type p. We can think of a Line-Segment being defined as
--
--  data LineSegment d p r = LineSegment (EndPoint (Point d r :+ p))
--                                       (EndPoint (Point d r :+ p))
newtype LineSegment d p r = GLineSegment { _unLineSeg :: Interval p (Point d r)}

makeLenses ''LineSegment

-- | Pattern that essentially models the line segment as a:
--
-- data LineSegment d p r = LineSegment (EndPoint (Point d r :+ p))
--                                      (EndPoint (Point d r :+ p))
pattern LineSegment           :: EndPoint (Point d r :+ p)
                              -> EndPoint (Point d r :+ p)
                              -> LineSegment d p r
pattern LineSegment       s t = GLineSegment (Interval s t)

-- | Gets the start and end point, but forgetting if they are open or closed.
pattern LineSegment'          :: Point d r :+ p
                              -> Point d r :+ p
                              -> LineSegment d p r
pattern LineSegment'      s t <- ((^.start) &&& (^.end) -> (s,t))


pattern ClosedLineSegment     :: Point d r :+ p
                              -> Point d r :+ p
                              -> LineSegment d p r
pattern ClosedLineSegment s t = GLineSegment (ClosedInterval s t)

type instance Dimension (LineSegment d p r) = d
type instance NumType   (LineSegment d p r) = r

instance HasStart (LineSegment d p r) where
  type StartCore  (LineSegment d p r) = Point d r
  type StartExtra (LineSegment d p r) = p
  start = unLineSeg.start

instance HasEnd (LineSegment d p r) where
  type EndCore  (LineSegment d p r) = Point d r
  type EndExtra (LineSegment d p r) = p
  end = unLineSeg.end


_SubLine :: (Fractional r, Eq r, Arity d) => Iso' (LineSegment d p r) (SubLine d p r)
_SubLine = iso segment2SubLine subLineToSegment
{-# INLINE _SubLine #-}

segment2SubLine    :: (Fractional r, Eq r, Arity d)
                   => LineSegment d p r -> SubLine d p r
segment2SubLine ss = SubLine l (Interval s e)
  where
    l = supportingLine ss
    f = flip toOffset l
    (Interval p q)  = ss^.unLineSeg

    s = p&unEndPoint.core %~ f
    e = q&unEndPoint.core %~ f

subLineToSegment    :: (Num r, Arity d) => SubLine d p r -> LineSegment d p r
subLineToSegment sl = let (Interval s' e') = (fixEndPoints sl)^.subRange
                          s = s'&unEndPoint %~ (^.extra)
                          e = e'&unEndPoint %~ (^.extra)
                      in LineSegment s e

instance (Num r, Arity d) => HasSupportingLine (LineSegment d p r) where
  supportingLine s = lineThrough (s^.start.core) (s^.end.core)


instance (Show r, Show p, Arity d) => Show (LineSegment d p r) where
  show ~(LineSegment p q) = concat ["LineSegment (", show p, ") (", show q, ")"]

deriving instance (Eq r, Eq p, Arity d)     => Eq (LineSegment d p r)
-- deriving instance (Ord r, Ord p, Arity d)   => Ord (LineSegment d p r)
deriving instance Arity d                   => Functor (LineSegment d p)

instance PointFunctor (LineSegment d p) where
  pmap f ~(LineSegment s e) = LineSegment (s&unEndPoint %~ first f)
                                          (e&unEndPoint %~ first f)

instance Arity d => IsBoxable (LineSegment d p r) where
  boundingBox l = boundingBox (l^.start.core) <> boundingBox (l^.end.core)

instance (Num r, AlwaysTruePFT d) => IsTransformable (LineSegment d p r) where
  transformBy = transformPointFunctor

instance Arity d => Bifunctor (LineSegment d) where
  bimap f g (GLineSegment i) = GLineSegment $ bimap f (fmap g) i



-- ** Converting between Lines and LineSegments

-- | Directly convert a line into a line segment.
toLineSegment            :: (Monoid p, Num r, Arity d) => Line d r -> LineSegment d p r
toLineSegment (Line p v) = ClosedLineSegment (p       :+ mempty)
                                             (p .+^ v :+ mempty)

-- *** Intersecting LineSegments

type instance IntersectionOf (LineSegment 2 p r) (LineSegment 2 p r) = [ NoIntersection
                                                                       , Point 2 r
                                                                       , LineSegment 2 p r
                                                                       ]

type instance IntersectionOf (LineSegment 2 p r) (Line 2 r) = [ NoIntersection
                                                              , Point 2 r
                                                              , LineSegment 2 p r
                                                              ]


instance (Ord r, Fractional r) =>
         (LineSegment 2 p r) `IsIntersectableWith` (LineSegment 2 p r) where
  nonEmptyIntersection = defaultNonEmptyIntersection

  a `intersect` b = match ((a^._SubLine) `intersect` (b^._SubLine)) $
         (H coRec)
      :& (H coRec)
      :& (H $ coRec . subLineToSegment)
      :& RNil


instance (Ord r, Fractional r) =>
         (LineSegment 2 p r) `IsIntersectableWith` (Line 2 r) where
  nonEmptyIntersection = defaultNonEmptyIntersection

  ~s@(LineSegment p q) `intersect` l = let f  = bimap (fmap Val) (const ())
                                           s' = LineSegment (p&unEndPoint %~ f)
                                                            (q&unEndPoint %~ f)
                                    in match ((s'^._SubLine) `intersect` (fromLine l)) $
         (H   coRec)
      :& (H $ coRec . fmap (_unUnBounded))
      :& (H $ const (coRec s))
      :& RNil

-- * Functions on LineSegments

-- | Test if a point lies on a line segment.
--
-- >>> (point2 1 0) `onSegment` (ClosedLineSegment (origin :+ ()) (point2 2 0 :+ ()))
-- True
-- >>> (point2 1 1) `onSegment` (ClosedLineSegment (origin :+ ()) (point2 2 0 :+ ()))
-- False
-- >>> (point2 5 0) `onSegment` (ClosedLineSegment (origin :+ ()) (point2 2 0 :+ ()))
-- False
-- >>> (point2 (-1) 0) `onSegment` (ClosedLineSegment (origin :+ ()) (point2 2 0 :+ ()))
-- False
-- >>> (point2 1 1) `onSegment` (ClosedLineSegment (origin :+ ()) (point2 3 3 :+ ()))
-- True
--
-- Note that the segments are assumed to be closed. So the end points lie on the segment.
--
-- >>> (point2 2 0) `onSegment` (ClosedLineSegment (origin :+ ()) (point2 2 0 :+ ()))
-- True
-- >>> origin `onSegment` (ClosedLineSegment (origin :+ ()) (point2 2 0 :+ ()))
-- True
--
--
-- This function works for arbitrary dimensons.
--
-- >>> (point3 1 1 1) `onSegment` (ClosedLineSegment (origin :+ ()) (point3 3 3 3 :+ ()))
-- True
-- >>> (point3 1 2 1) `onSegment` (ClosedLineSegment (origin :+ ()) (point3 3 3 3 :+ ()))
-- False
onSegment       :: (Ord r, Fractional r, Arity d)
                => Point d r -> LineSegment d p r -> Bool
p `onSegment` l = let s          = l^.start.core
                      t          = l^.end.core
                      inRange' x = 0 <= x && x <= 1
                  in maybe False inRange' $ scalarMultiple (p .-. s) (t .-. s)


-- | The left and right end point (or left below right if they have equal x-coords)
orderedEndPoints   :: Ord r => LineSegment 2 p r -> (Point 2 r :+ p, Point 2 r :+ p)
orderedEndPoints s = if pc <= qc then (p, q) else (q,p)
  where
    p@(pc :+ _) = s^.start
    q@(qc :+ _) = s^.end


-- | Length of the line segment
segmentLength                     :: (Arity d, Floating r) => LineSegment d p r -> r
segmentLength ~(LineSegment' p q) = distanceA (p^.core) (q^.core)


-- | Squared distance from the point to the Segment s. The same remark as for
-- the 'sqDistanceToSegArg' applies here.
sqDistanceToSeg   :: (Arity d, Fractional r, Ord r) => Point d r -> LineSegment d p r -> r
sqDistanceToSeg p = fst . sqDistanceToSegArg p


-- | Squared distance from the point to the Segment s, and the point on s
-- realizing it.  Note that if the segment is *open*, the closest point
-- returned may be one of the (open) end points, even though technically the
-- end point does not lie on the segment. (The true closest point then lies
-- arbitrarily close to the end point).
sqDistanceToSegArg     :: (Arity d, Fractional r, Ord r)
                       => Point d r -> LineSegment d p r -> (r, Point d r)
sqDistanceToSegArg p s = let m  = sqDistanceToArg p (supportingLine s)
                             xs = m : map (\(q :+ _) -> (qdA p q, q)) [s^.start, s^.end]
                         in   F.minimumBy (comparing fst)
                            . filter (flip onSegment s . snd) $ xs

-- | flips the start and end point of the segment
flipSegment   :: LineSegment d p r -> LineSegment d p r
flipSegment s = let p = s^.start
                    q = s^.end
                in (s&start .~ q)&end .~ p

-- testSeg :: LineSegment 2 () Rational
-- testSeg = LineSegment (Open $ ext origin)  (Closed $ ext (point2 10 0))

-- horL' :: Line 2 Rational
-- horL' = horizontalLine 0

-- testI = testSeg `intersect` horL'


-- ff = bimap (fmap Val) (const ())

-- ss' = let (LineSegment p q) = testSeg in
--       LineSegment (p&unEndPoint %~ ff)
--                   (q&unEndPoint %~ ff)

-- ss'' = ss'^._SubLine
