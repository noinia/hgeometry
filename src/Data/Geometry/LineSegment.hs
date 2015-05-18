{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.LineSegment where

import           Control.Applicative
import           Control.Lens
import           Data.Bifunctor
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.Interval
import           Data.Geometry.Line.Internal
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import qualified Data.List as L
import           Data.Maybe(maybe)
import           Data.Ord(comparing)
import           Data.Semigroup
import           Linear.Affine(Affine(..),distanceA)
import           Linear.Vector((*^))

--------------------------------------------------------------------------------
-- * d-dimensional LineSegments

newtype GLineSegment d s t p r = GLineSegment { _unLineSeg :: GInterval s t p (Point d r) }
pattern LineSegment s t = GLineSegment (Range s t)

makeLenses ''GLineSegment

type SymLineSegment d t = GLineSegment d t t
type LineSegment d = SymLineSegment d Closed

-- -- | Line segments. LineSegments have a start and end point, both of which may
-- -- contain additional data of type p.
-- newtype LineSegment d p r = LineSeg { _unLineSeg :: Interval p (Point d r) }
-- pattern LineSegment s t = LineSeg (Interval s t)

instance HasStart (GLineSegment d s t p r) where
  type StartCore  (GLineSegment d s t p r) = Point d r
  type StartExtra (GLineSegment d s t p r) = p
  start = unLineSeg.lower.unEndPoint

    -- lens (_start . _unLineSeg) (\(LineSegment _ t) s -> LineSegment s t)

instance HasEnd (GLineSegment d s t p r) where
  type EndCore  (GLineSegment d s t p r) = Point d r
  type EndExtra (GLineSegment d s t p r) = p
  end = unLineSeg.upper.unEndPoint

--    lens (_end . _unLineSeg) (\(LineSegment s _) t -> LineSegment s t)

instance (Num r, Arity d) => HasSupportingLine (GLineSegment d s t p r) where
  supportingLine (LineSegment (p :+ _) (q :+ _)) = lineThrough p q

deriving instance (AlwaysTruePrettyShow s t
                  ,Show r, Show p, Arity d) => Show (GLineSegment d s t p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq (GLineSegment d s t p r)
deriving instance (Ord r, Ord p, Arity d)   => Ord (GLineSegment d s t p r)
-- deriving instance Arity d                   => Functor (GLineSegment d p)
type instance Dimension (GLineSegment d s t p r) = d
type instance NumType   (GLineSegment d s t p r) = r

instance PointFunctor (GLineSegment d s t p) where
  pmap f (LineSegment s e) = LineSegment (first f s) (first f e)

-- | Only for closed segs, since boxes are closed
instance Arity d => IsBoxable (LineSegment d p r) where
  boundingBox l = boundingBoxList [l^.start.core, l^.end.core]

-- instance (Num r, AlwaysTruePFT d) => IsTransformable (LineSegment d p r) where
--   transformBy = transformPointFunctor








-- ** Converting between Lines and LineSegments

toLineSegment            :: (Monoid p, Num r, Arity d) => Line d r -> LineSegment d p r
toLineSegment (Line p v) = LineSegment (p       :+ mempty)
                                       (p .+^ v :+ mempty)

-- *** Intersecting LineSegments

-- instance (Ord r, Fractional r) =>
--          (LineSegment 2 p r) `IsIntersectableWith` (LineSegment 2 p r) where

--   data Intersection (LineSegment 2 p r) (LineSegment 2 p r) =
--         OverlappingSegment         !(LineSegment 2 p r)
--       | LineSegLineSegIntersection !(Point 2 r)
--       | NoIntersection
--       deriving (Show,Eq)

--   nonEmptyIntersection NoIntersection = False
--   nonEmptyIntersection _              = True

--   a@(LineSegment p q) `intersect` b@(LineSegment s t) = case la `intersect` lb of
--       SameLine _                                ->
--           maybe NoIntersection OverlappingSegment $ overlap a b
--       LineLineIntersection r | onBothSegments r -> LineSegLineSegIntersection r
--       _                                         -> NoIntersection
--     where
--       la = supportingLine a
--       lb = supportingLine b
--       onBothSegments r = onSegment r a && onSegment r b

-- instance (Ord r, Fractional r) =>
--          (LineSegment 2 p r) `IsIntersectableWith` (Line 2 r) where
--   data Intersection (LineSegment 2 p r) (Line 2 r) =
--            LineContainsSegment !(LineSegment 2 p r)
--          | LineLineSegmentIntersection !(Point 2 r)
--          | NoLineLineSegmentIntersection
--          deriving (Show,Eq)

--   nonEmptyIntersection NoLineLineSegmentIntersection = False
--   nonEmptyIntersection _                             = True

--   s `intersect` l = case (supportingLine s) `intersect` l of
--     SameLine _                               -> LineContainsSegment s
--     LineLineIntersection p | p `onSegment` s -> LineLineSegmentIntersection p
--     _                                        -> NoLineLineSegmentIntersection


-- * Functions on LineSegments

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
                      inRange x = 0 <= x && x <= 1
                  in maybe False inRange $ scalarMultiple (p .-. s) (t .-. s)


-- -- | Compute the overlap between the two segments (if they overlap/intersect)
-- overlap     :: (Ord r, Fractional r, Arity d)
--             => LineSegment d p r -> LineSegment d p r -> Maybe (LineSegment d p r)
-- overlap l m = mim >>= \im -> case il `intersect` im of
--     IntervalIntersection (Interval s e) -> Just $ LineSegment (s^.extra) (e^.extra)
--     NoOverlap                           -> Nothing
--   where
--     p = l^.start
--     q = l^.end
--     r = m^.start
--     s = m^.end

--     u = q^.core .-. p^.core

--     -- lineseg l corresp to an interval from 0 1
--     il = Interval (0 :+ p) (1 :+ q)


--     -- let lambda x denote the scalar s.t. x = p + (lambda x) *^ u
--     --
--     -- lambda' computes lambda' and pairs it with the associated point.
--     -- lambda  :: (Point d r :+ extra) -> Maybe (r :+ (Point d r :+ extra)
--     lambda' x = (:+ x) <$> scalarMultiple u (x^.core .-. p^.core)

--     -- lineseg m corresponds to an interval
--     -- [min (lambda r, lambda s), max (lambda r, lambda s)]
--     --
--     -- mim denotes this interval, assuming it exists (i.e. that is,
--     -- assuming r and s are indeed colinear with pq.
--     mim = mapM lambda' [r, s] >>= (f . L.sortBy (comparing (^.core)))

--     -- Make sure we have two elems, s.t. both r and s are colinear with pq
--     f [a,b] = Just $ Interval a b
--     f _     = Nothing


-- | The left and right end point (or left below right if they have equal x-coords)
orderedEndPoints   :: Ord r => LineSegment 2 p r -> (Point 2 r :+ p, Point 2 r :+ p)
orderedEndPoints s = if pc <= qc then (p, q) else (q,p)
  where
    p@(pc :+ _) = s^.start
    q@(qc :+ _) = s^.end


-- | Length of the line segment
segmentLength                   :: (Arity d, Floating r) => LineSegment d p r -> r
segmentLength (LineSegment p q) = distanceA (p^.core) (q^.core)
