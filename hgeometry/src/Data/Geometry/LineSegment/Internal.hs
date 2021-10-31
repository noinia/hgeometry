{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.LineSegment.Internal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Line segment data type and some basic functions on line segments
--
--------------------------------------------------------------------------------
module Data.Geometry.LineSegment.Internal
  ( LineSegment(LineSegment, LineSegment', ClosedLineSegment, OpenLineSegment)
  , endPoints

  , _SubLine
  , module Data.Geometry.Interval


  , toLineSegment
  , onSegment, onSegment2
  , orderedEndPoints
  , segmentLength
  , sqSegmentLength
  , sqDistanceToSeg, sqDistanceToSegArg
  , flipSegment

  , interpolate
  , validSegment
  , sampleLineSegment
  ) where

import           Control.Arrow ((&&&))
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.Random
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Box.Internal
import           Data.Geometry.Interval hiding (width, midPoint)
import           Data.Geometry.Line.Internal
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.SubLine
import           Data.Geometry.Transformation.Internal
import           Data.Geometry.Vector
import           Data.Ord (comparing)
import           Data.Vinyl
import           Data.Vinyl.CoRec
import           GHC.TypeLits
import           Test.QuickCheck (Arbitrary(..), suchThatMap)
import           Text.Read

--------------------------------------------------------------------------------
-- * d-dimensional LineSegments


-- | Line segments. LineSegments have a start and end point, both of which may
-- contain additional data of type p. We can think of a Line-Segment being defined as
--
--
-- >>>  data LineSegment d p r = LineSegment (EndPoint (Point d r :+ p)) (EndPoint (Point d r :+ p))
--
-- it is assumed that the two endpoints of the line segment are disjoint. This is not checked.
newtype LineSegment d p r = GLineSegment { _unLineSeg :: Interval p (Point d r) }

makeLenses ''LineSegment


pattern LineSegment           :: EndPoint (Point d r :+ p)
                              -> EndPoint (Point d r :+ p)
                              -> LineSegment d p r
pattern LineSegment       s t = GLineSegment (Interval s t)
{-# COMPLETE LineSegment #-}

-- | Gets the start and end point, but forgetting if they are open or closed.
pattern LineSegment'          :: Point d r :+ p
                              -> Point d r :+ p
                              -> LineSegment d p r
pattern LineSegment'      s t <- ((^.start) &&& (^.end) -> (s,t))
{-# COMPLETE LineSegment' #-}

pattern ClosedLineSegment     :: Point d r :+ p -> Point d r :+ p -> LineSegment d p r
pattern ClosedLineSegment s t = GLineSegment (ClosedInterval s t)
{-# COMPLETE ClosedLineSegment #-}

pattern OpenLineSegment     :: Point d r :+ p -> Point d r :+ p -> LineSegment d p r
pattern OpenLineSegment s t = GLineSegment (OpenInterval s t)
{-# COMPLETE OpenLineSegment #-}



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

instance (Arbitrary r, Arbitrary p, Eq r, Arity d) => Arbitrary (LineSegment d p r) where
  arbitrary = suchThatMap ((,) <$> arbitrary <*> arbitrary)
                          (uncurry validSegment)


deriving instance (Arity d, NFData r, NFData p) => NFData (LineSegment d p r)

sampleLineSegment :: (Arity d, RandomGen g, Random r) => Rand g (LineSegment d () r)
sampleLineSegment = do
  a <- ext <$> getRandom
  a' <- getRandom
  b <- ext <$> getRandom
  b' <- getRandom
  pure $ LineSegment (if a' then Open a else Closed a) (if b' then Open b else Closed b)


{- HLINT ignore endPoints -}
-- | Traversal to access the endpoints. Note that this traversal
-- allows you to change more or less everything, even the dimension
-- and the numeric type used, but it preservers if the segment is open
-- or closed.
endPoints :: Traversal (LineSegment d p r) (LineSegment d' q s)
                       (Point d r :+ p)    (Point d' s :+ q)
endPoints = \f (LineSegment p q) -> LineSegment <$> traverse f p
                                                <*> traverse f q

_SubLine :: (Num r, Arity d) => Iso' (LineSegment d p r) (SubLine d p r r)
_SubLine = iso segment2SubLine subLineToSegment
{-# INLINE _SubLine #-}

segment2SubLine    :: (Num r, Arity d)
                   => LineSegment d p r -> SubLine d p r r
segment2SubLine ss = SubLine (Line p (q .-. p)) (Interval s e)
  where
    p = ss^.start.core
    q = ss^.end.core
    (Interval a b)  = ss^.unLineSeg
    s = a&unEndPoint.core .~ 0
    e = b&unEndPoint.core .~ 1

{- HLINT ignore subLineToSegment -}
subLineToSegment    :: (Num r, Arity d) => SubLine d p r r -> LineSegment d p r
subLineToSegment sl = let Interval s' e' = (fixEndPoints sl)^.subRange
                          s = s'&unEndPoint %~ (^.extra)
                          e = e'&unEndPoint %~ (^.extra)
                      in LineSegment s e

instance (Num r, Arity d) => HasSupportingLine (LineSegment d p r) where
  supportingLine s = lineThrough (s^.start.core) (s^.end.core)


instance (Show r, Show p, Arity d) => Show (LineSegment d p r) where
  showsPrec d (LineSegment p' q') = case (p',q') of
      (Closed p, Closed q) -> f "ClosedLineSegment" p q
      (Open p, Open q)     -> f "OpenLineSegment"   p q
      (p,q)                -> f "LineSegment"       p q
    where
      app_prec = 10
      f        :: (Show a, Show b) => String -> a -> b -> String -> String
      f cn p q = showParen (d > app_prec) $
                     showString cn . showString " "
                   . showsPrec (app_prec+1) p
                   . showString " "
                   . showsPrec (app_prec+1) q

instance (Read r, Read p, Arity d) => Read (LineSegment d p r) where
  readPrec = parens $ (prec app_prec $ do
                                  Ident "ClosedLineSegment" <- lexP
                                  p <- step readPrec
                                  q <- step readPrec
                                  return (ClosedLineSegment p q))
                       +++
                       (prec app_prec $ do
                                  Ident "OpenLineSegment" <- lexP
                                  p <- step readPrec
                                  q <- step readPrec
                                  return (OpenLineSegment p q))
                       +++
                       (prec app_prec $ do
                                  Ident "LineSegment" <- lexP
                                  p <- step readPrec
                                  q <- step readPrec
                                  return (LineSegment p q))
    where app_prec = 10


deriving instance (Eq r, Eq p, Arity d)     => Eq (LineSegment d p r)
-- deriving instance (Ord r, Ord p, Arity d)   => Ord (LineSegment d p r)
deriving instance Arity d                   => Functor (LineSegment d p)

instance PointFunctor (LineSegment d p) where
  pmap f ~(LineSegment s e) = LineSegment (s&unEndPoint.core %~ f)
                                          (e&unEndPoint.core %~ f)

instance Arity d => IsBoxable (LineSegment d p r) where
  boundingBox l = boundingBox (l^.start.core) <> boundingBox (l^.end.core)

instance (Fractional r, Arity d, Arity (d + 1)) => IsTransformable (LineSegment d p r) where
  transformBy = transformPointFunctor

instance Arity d => Bifunctor (LineSegment d) where
  bimap f g (GLineSegment i) = GLineSegment $ bimap f (fmap g) i



-- ** Converting between Lines and LineSegments

-- | Directly convert a line into a line segment.
toLineSegment            :: (Monoid p, Num r, Arity d) => Line d r -> LineSegment d p r
toLineSegment (Line p v) = ClosedLineSegment (p       :+ mempty)
                                             (p .+^ v :+ mempty)

-- *** Intersecting LineSegments

type instance IntersectionOf (Point d r) (LineSegment d p r) = [ NoIntersection
                                                               , Point d r
                                                               ]

type instance IntersectionOf (LineSegment 2 p r) (LineSegment 2 p r) = [ NoIntersection
                                                                       , Point 2 r
                                                                       , LineSegment 2 p r
                                                                       ]

type instance IntersectionOf (LineSegment 2 p r) (Line 2 r) = [ NoIntersection
                                                              , Point 2 r
                                                              , LineSegment 2 p r
                                                              ]


instance {-# OVERLAPPING #-} (Ord r, Num r)
         => Point 2 r `HasIntersectionWith` LineSegment 2 p r where
  intersects = onSegment2

instance {-# OVERLAPPING #-} (Ord r, Num r)
         => Point 2 r `IsIntersectableWith` LineSegment 2 p r where
  nonEmptyIntersection = defaultNonEmptyIntersection
  p `intersect` seg | p `intersects` seg = coRec p
                    | otherwise          = coRec NoIntersection


instance {-# OVERLAPPABLE #-} (Ord r, Fractional r, Arity d)
         => Point d r `HasIntersectionWith` LineSegment d p r where
  intersects = onSegment

instance {-# OVERLAPPABLE #-} (Ord r, Fractional r, Arity d)
         => Point d r `IsIntersectableWith` LineSegment d p r where
  nonEmptyIntersection = defaultNonEmptyIntersection
  p `intersect` seg | p `intersects` seg = coRec p
                    | otherwise          = coRec NoIntersection

-- | Test if a point lies on a line segment.
--
-- As a user, you should typically just use 'intersects' instead.
onSegment :: (Ord r, Fractional r, Arity d) => Point d r -> LineSegment d p r -> Bool
p `onSegment` (LineSegment up vp) =
      maybe False inRange' (scalarMultiple (p .-. u) (v .-. u))
    where
      u = up^.unEndPoint.core
      v = vp^.unEndPoint.core

      atMostUpperBound  = if isClosed vp then (<= 1) else (< 1)
      atLeastLowerBound = if isClosed up then (0 <=) else (0 <)

      inRange' x = atLeastLowerBound x && atMostUpperBound x
  -- the type of test we use for the 2D version might actually also
  -- work in higher dimensions that might allow us to drop the
  -- Fractional constraint

instance (Ord r, Fractional r) =>
         LineSegment 2 p r `HasIntersectionWith` LineSegment 2 p r

instance (Ord r, Fractional r) =>
         LineSegment 2 p r `IsIntersectableWith` LineSegment 2 p r where
  nonEmptyIntersection = defaultNonEmptyIntersection

  a `intersect` b = match ((a^._SubLine) `intersect` (b^._SubLine)) $
         H coRec
      :& H coRec
      :& H (coRec . subLineToSegment)
      :& RNil

instance (Ord r, Fractional r) =>
         LineSegment 2 p r `HasIntersectionWith` Line 2 r where

instance (Ord r, Fractional r) =>
         LineSegment 2 p r `IsIntersectableWith` Line 2 r where
  nonEmptyIntersection = defaultNonEmptyIntersection

  s `intersect` l = let ubSL = s^._SubLine.re _unBounded.to dropExtra
                    in match (ubSL `intersect` fromLine l) $
                            H  coRec
                         :& H  coRec
                         :& H (const (coRec s))
                         :& RNil



-- * Functions on LineSegments

-- | Test if a point lies on a line segment.
--
-- >>> (Point2 1 0) `onSegment2` (ClosedLineSegment (origin :+ ()) (Point2 2 0 :+ ()))
-- True
-- >>> (Point2 1 1) `onSegment2` (ClosedLineSegment (origin :+ ()) (Point2 2 0 :+ ()))
-- False
-- >>> (Point2 5 0) `onSegment2` (ClosedLineSegment (origin :+ ()) (Point2 2 0 :+ ()))
-- False
-- >>> (Point2 (-1) 0) `onSegment2` (ClosedLineSegment (origin :+ ()) (Point2 2 0 :+ ()))
-- False
-- >>> (Point2 1 1) `onSegment2` (ClosedLineSegment (origin :+ ()) (Point2 3 3 :+ ()))
-- True
-- >>> (Point2 2 0) `onSegment2` (ClosedLineSegment (origin :+ ()) (Point2 2 0 :+ ()))
-- True
-- >>> origin `onSegment2` (ClosedLineSegment (origin :+ ()) (Point2 2 0 :+ ()))
-- True
onSegment2                          :: (Ord r, Num r)
                                    => Point 2 r -> LineSegment 2 p r -> Bool
p `onSegment2` s@(LineSegment u v) = case ccw' (ext p) (u^.unEndPoint) (v^.unEndPoint) of
    CoLinear -> let su = p `onSide` lu
                    sv = p `onSide` lv
                in su /= sv
                && ((su == OnLine) `implies` isClosed u)
                && ((sv == OnLine) `implies` isClosed v)
    _        -> False
  where
    (Line _ w) = perpendicularTo $ supportingLine s
    lu = Line (u^.unEndPoint.core) w
    lv = Line (v^.unEndPoint.core) w

    a `implies` b = b || not a


-- | The left and right end point (or left below right if they have equal x-coords)
orderedEndPoints   :: Ord r => LineSegment 2 p r -> (Point 2 r :+ p, Point 2 r :+ p)
orderedEndPoints s = if pc <= qc then (p, q) else (q,p)
  where
    p@(pc :+ _) = s^.start
    q@(qc :+ _) = s^.end


-- | Length of the line segment
segmentLength                     :: (Arity d, Floating r) => LineSegment d p r -> r
segmentLength ~(LineSegment' p q) = distanceA (p^.core) (q^.core)

sqSegmentLength                     :: (Arity d, Num r) => LineSegment d p r -> r
sqSegmentLength ~(LineSegment' p q) = qdA (p^.core) (q^.core)

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
-- testSeg = LineSegment (Open $ ext origin)  (Closed $ ext (Point2 10 0))

-- horL' :: Line 2 Rational
-- horL' = horizontalLine 0

-- testI = testSeg `intersect` horL'


-- ff = bimap (fmap Val) (const ())

-- ss' = let (LineSegment p q) = testSeg in
--       LineSegment (p&unEndPoint %~ ff)
--                   (q&unEndPoint %~ ff)

-- ss'' = ss'^._SubLine

-- | Linearly interpolate the two endpoints with a value in the range [0,1]
--
-- >>> interpolate 0.5 $ ClosedLineSegment (ext $ origin) (ext $ Point2 10.0 10.0)
-- Point2 5.0 5.0
-- >>> interpolate 0.1 $ ClosedLineSegment (ext $ origin) (ext $ Point2 10.0 10.0)
-- Point2 1.0 1.0
-- >>> interpolate 0 $ ClosedLineSegment (ext $ origin) (ext $ Point2 10.0 10.0)
-- Point2 0.0 0.0
-- >>> interpolate 1 $ ClosedLineSegment (ext $ origin) (ext $ Point2 10.0 10.0)
-- Point2 10.0 10.0
interpolate                      :: (Fractional r, Arity d) => r -> LineSegment d p r -> Point d r
interpolate t (LineSegment' p q) = Point $ (asV p ^* (1-t)) ^+^ (asV q ^* t)
  where
    asV = (^.core.vector)


-- | smart constructor that creates a valid segment, i.e. it validates
-- that the endpoints are disjoint.
validSegment     :: (Eq r, Arity d)
                 => EndPoint (Point d r :+ p) -> EndPoint (Point d r :+ p)
                 -> Maybe (LineSegment d p r)
validSegment u v = let s = LineSegment u v
                   in if s^.start.core /= s^.end.core then Just s else Nothing
