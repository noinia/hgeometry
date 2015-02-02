{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE TemplateHaskell  #-}
module Data.Geometry.Line where


import           Control.Applicative
import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.List as L
import           Data.Monoid
import           Data.Ord(comparing)
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.Interval
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import qualified Data.Seq2 as S2
import           Linear.Affine(Affine(..))
import           Linear.Vector((*^))

--------------------------------------------------------------------------------

-- | A line is given by an anchor point and a vector indicating the
-- direction.
data Line d r = Line { _anchorPoint :: Point  d r
                     , _direction   :: Vector d r
                     }
makeLenses ''Line

deriving instance (Show r, Arity d) => Show    (Line d r)
deriving instance Arity d           => Functor (Line d)

type instance Dimension (Line d r) = d
type instance NumType   (Line d r) = r

-- | A line may be constructed from two points.
lineThrough     :: (Num r, Arity d) => Point d r -> Point d r -> Line d r
lineThrough p q = Line p (q .-. p)

verticalLine   :: Num r => r -> Line 2 r
verticalLine x = Line (point2 x 0) (v2 0 1)

horizontalLine   :: Num r => r -> Line 2 r
horizontalLine y = Line (point2 0 y) (v2 1 0)


-- | Test if two lines are identical, meaning; if they have exactly the same
-- anchor point and directional vector.
isIdenticalTo                         :: (Eq r, Arity d) => Line d r -> Line d r -> Bool
(Line p u) `isIdenticalTo` (Line q v) = (p,u) == (q,v)


-- | Test if the two lines are parallel.
--
-- >>> lineThrough origin (point2 1 0) `isParallelTo` lineThrough (point2 1 1) (point2 2 1)
-- True
-- >>> lineThrough origin (point2 1 0) `isParallelTo` lineThrough (point2 1 1) (point2 2 2)
-- False
isParallelTo                         :: (Eq r, Fractional r, Arity d)
                                     => Line d r -> Line d r -> Bool
(Line _ u) `isParallelTo` (Line _ v) = u `isScalarMultipleOf` v
  -- TODO: Maybe use a specialize pragma for 2D (see intersect instance for two lines.)


-- | Test if point p lies on line l
--
-- >>> origin `onLine` lineThrough origin (point2 1 0)
-- True
-- >>> point2 10 10 `onLine` lineThrough origin (point2 2 2)
-- True
-- >>> point2 10 5 `onLine` lineThrough origin (point2 2 2)
-- False
onLine                :: (Eq r, Fractional r, Arity d) => Point d r -> Line d r -> Bool
p `onLine` (Line q v) = p == q || (p .-. q) `isScalarMultipleOf` v
  -- TODO: Maybe use a specialize pragma for 2D with an implementation using ccw



instance (Eq r, Fractional r) => (Line 2 r) `IsIntersectableWith` (Line 2 r) where

  data Intersection (Line 2 r) (Line 2 r) = SameLine             (Line 2 r)
                                          | LineLineIntersection (Point 2 r)
                                          | ParallelLines -- ^ No intersection
                                            deriving (Show)

  nonEmptyIntersection ParallelLines = False
  nonEmptyIntersection _             = True

  l@(Line p (Vector2 ux uy)) `intersect` m@(Line q v@(Vector2 vx vy))
      | areParallel = if q `onLine` l then SameLine l else ParallelLines
      | otherwise   = LineLineIntersection r
    where
      r = p .+^ alpha *^ v

      denom       = vy * ux - vx * uy
      areParallel = denom == 0
      -- Instead of using areParallel, we can also use the generic 'isParallelTo' function
      -- for lines of arbitrary dimension, but this is a bit more efficient.

      alpha        = (ux * (py - qy) + uy * (qx - px)) / denom

      Point2 px py = p
      Point2 qx qy = q

instance (Eq r, Fractional r) => Eq (Intersection (Line 2 r) (Line 2 r)) where
  (SameLine l)             == (SameLine m)             = case (l `intersect` m) of
                                                           SameLine _ -> True
                                                           _          -> False
  (LineLineIntersection p) == (LineLineIntersection q) = p == q
  ParallelLines            == ParallelLines            = True
  _                        == _                        = False


--------------------------------------------------------------------------------

-- | Line segments. LineSegments have a start and end point, both of which may
-- contain additional data of type p.
newtype LineSegment d p r = LineSeg { _unLineSeg :: Interval p (Point d r) }
pattern LineSegment s t = LineSeg (Interval s t)

instance HasStart (LineSegment d p r) where
  type StartCore  (LineSegment d p r) = Point d r
  type StartExtra (LineSegment d p r) = p
  start = lens (_start . _unLineSeg) (\(LineSegment _ t) s -> LineSegment s t)

instance HasEnd (LineSegment d p r) where
  type EndCore  (LineSegment d p r) = Point d r
  type EndExtra (LineSegment d p r) = p
  end = lens (_end . _unLineSeg) (\(LineSegment s _) t -> LineSegment s t)

deriving instance (Show r, Show p, Arity d) => Show (LineSegment d p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq (LineSegment d p r)
deriving instance (Ord r, Ord p, Arity d)   => Ord (LineSegment d p r)
deriving instance Arity d                   => Functor (LineSegment d p)
type instance Dimension (LineSegment d p r) = d
type instance NumType   (LineSegment d p r) = r

instance PointFunctor (LineSegment d p) where
  pmap f (LineSegment s e) = LineSegment (f <$> s) (f <$> e)

instance Arity d => IsBoxable (LineSegment d p r) where
  boundingBox l = boundingBoxList [l^.start.core, l^.end.core]

instance (Num r, AlwaysTruePFT d) => IsTransformable (LineSegment d p r) where
  transformBy = transformPointFunctor


toLineSegment            :: (Monoid p, Num r, Arity d) => Line d r -> LineSegment d p r
toLineSegment (Line p v) = LineSegment (p       :+ mempty)
                                       (p .+^ v :+ mempty)

toLine                                 :: (Num r, Arity d) => LineSegment d p r -> Line d r
toLine (LineSegment (p :+ _) (q :+ _)) = lineThrough p q


instance (Ord r, Fractional r) =>
         (LineSegment 2 p r) `IsIntersectableWith` (LineSegment 2 p r) where

  data Intersection (LineSegment 2 p r) (LineSegment 2 p r) =
        OverlappingSegment         (LineSegment 2 p r)
      | LineSegLineSegIntersection (Point 2 r)
      | NoIntersection
      deriving (Show,Eq)

  nonEmptyIntersection NoIntersection = False
  nonEmptyIntersection _              = True

  a@(LineSegment p q) `intersect` b@(LineSegment s t) = case la `intersect` lb of
      SameLine _                                ->
          maybe NoIntersection OverlappingSegment $ overlap a b
      LineLineIntersection r | onBothSegments r -> LineSegLineSegIntersection r
      _                                         -> NoIntersection
    where
      la = toLine a
      lb = toLine b
      onBothSegments r = onSegment r a && onSegment r b

instance (Ord r, Fractional r) =>
         (LineSegment 2 p r) `IsIntersectableWith` (Line 2 r) where
  data Intersection (LineSegment 2 p r) (Line 2 r) = LineContainsSegment (LineSegment 2 p r)
                                                   | LineLineSegmentIntersection (Point 2 r)
                                                   | NoLineLineSegmentIntersection
                                                   deriving (Show,Eq)

  nonEmptyIntersection NoLineLineSegmentIntersection = False
  nonEmptyIntersection _                             = True

  s `intersect` l = case (toLine s) `intersect` l of
    SameLine _                               -> LineContainsSegment s
    LineLineIntersection p | p `onSegment` s -> LineLineSegmentIntersection p
    _                                        -> NoLineLineSegmentIntersection


-- | Test if a point lies on a line segment.
--
-- >>> (point2 1 0) `onSegment` (LineSegment (origin :+ ()) (point2 2 0 :+ ()))
-- True
-- >>> (point2 1 1) `onSegment` (LineSegment (origin :+ ()) (point2 2 0 :+ ()))
-- False
-- >>> (point2 5 0) `onSegment` (LineSegment (origin :+ ()) (point2 2 0 :+ ()))
-- False
-- >>> (point2 (-1) 0) `onSegment` (LineSegment (origin :+ ()) (point2 2 0 :+ ()))
-- False
-- >>> (point2 1 1) `onSegment` (LineSegment (origin :+ ()) (point2 3 3 :+ ()))
-- True
--
-- Note that the segments are assumed to be closed. So the end points lie on the segment.
--
-- >>> (point2 2 0) `onSegment` (LineSegment (origin :+ ()) (point2 2 0 :+ ()))
-- True
-- >>> origin `onSegment` (LineSegment (origin :+ ()) (point2 2 0 :+ ()))
-- True
--
--
-- This function works for arbitrary dimensons.
--
-- >>> (point3 1 1 1) `onSegment` (LineSegment (origin :+ ()) (point3 3 3 3 :+ ()))
-- True
-- >>> (point3 1 2 1) `onSegment` (LineSegment (origin :+ ()) (point3 3 3 3 :+ ()))
-- False
onSegment       :: (Ord r, Fractional r, Arity d)
                => Point d r -> LineSegment d p r -> Bool
p `onSegment` l = let s         = l^.start.core
                      t         = l^.end.core
                      inRange x = (fromIntegral 0) <= x && x <= (fromIntegral 1)
                  in maybe False inRange $ scalarMultiple (p .-. s) (t .-. s)



overlap     :: (Ord r, Fractional r, Arity d)
            => LineSegment d p r -> LineSegment d p r -> Maybe (LineSegment d p r)
overlap l m = mim >>= \im -> case il `intersect` im of
    IntervalIntersection (Interval s e) -> Just $ LineSegment (s^.extra) (e^.extra)
    NoOverlap                           -> Nothing
  where
    p = l^.start
    q = l^.end
    r = m^.start
    s = m^.end

    u = q^.core .-. p^.core

    -- lineseg l corresp to an interval from 0 1
    il = Interval (fromIntegral 0 :+ p) (fromIntegral 1 :+ q)


    -- let lambda x denote the scalar s.t. x = p + (lambda x) *^ u
    --
    -- lambda' computes lambda' and pairs it with the associated point.
    -- lambda  :: (Point d r :+ extra) -> Maybe (r :+ (Point d r :+ extra)
    lambda' x = (:+ x) <$> scalarMultiple u (x^.core .-. p^.core)

    -- lineseg m corresponds to an interval
    -- [min (lambda r, lambda s), max (lambda r, lambda s)]
    --
    -- mim denotes this interval, assuming it exists (i.e. that is,
    -- assuming r and s are indeed colinear with pq.
    mim = mapM lambda' [r, s] >>= (f . L.sortBy (comparing (^.core)))

    -- Make sure we have two elems, s.t. both r and s are colinear with pq
    f [a,b] = Just $ Interval a b
    f _     = Nothing



-- | Lines are transformable, via line segments
instance (Num r, AlwaysTruePFT d) => IsTransformable (Line d r) where
  transformBy t = toLine . transformPointFunctor t . toLineSegment'
    where
      toLineSegment' :: (Num r, Arity d) => Line d r -> LineSegment d () r
      toLineSegment' = toLineSegment



-- | The left and right end point (or left below right if they have equal x-coords)
orderedEndPoints   :: Ord r => LineSegment 2 p r -> (Point 2 r :+ p, Point 2 r :+ p)
orderedEndPoints s = if pc <= qc then (p, q) else (q,p)
  where
    p@(pc :+ _) = s^.start
    q@(qc :+ _) = s^.end

--------------------------------------------------------------------------------

-- | A Poly line in R^d
newtype PolyLine d p r = PolyLine { _points :: S2.Seq2 (Point d r :+ p) }
makeLenses ''PolyLine

deriving instance (Show r, Show p, Arity d) => Show    (PolyLine d p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq      (PolyLine d p r)
deriving instance (Ord r, Ord p, Arity d)   => Ord     (PolyLine d p r)
deriving instance Arity d                   => Functor (PolyLine d p)
type instance Dimension (PolyLine d p r) = d
type instance NumType   (PolyLine d p r) = r

instance Arity d => IsBoxable (PolyLine d p r) where
  boundingBox = boundingBoxList . toListOf (points.traverse.core)

instance (Num r, AlwaysTruePFT d) => IsTransformable (PolyLine d p r) where
  transformBy = transformPointFunctor

instance PointFunctor (PolyLine d p) where
  pmap f = over points (fmap (fmap f))


-- | pre: The input list contains at least two points
fromPoints :: (Monoid p) => [Point d r] -> PolyLine d p r
fromPoints = PolyLine . S2.fromList . map (\p -> p :+ mempty)
