{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.SubLine
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- SubLine; a part of a line
--
--------------------------------------------------------------------------------
module Data.Geometry.SubLine where

import           Control.Lens
import           Data.Bifunctor
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Interval
import           Data.Geometry.Line.Internal
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import qualified Data.Traversable as T
import           Data.UnBounded
import           Data.Vinyl
import           Data.Vinyl.CoRec
import           Test.QuickCheck(Arbitrary(..))

import           Data.Ratio

--------------------------------------------------------------------------------

-- | Part of a line. The interval is ranged based on the vector of the
-- line l, and s.t.t zero is the anchorPoint of l.
data SubLine d p s r = SubLine { _line     :: Line d r
                               , _subRange :: Interval p s
                               }
makeLenses ''SubLine

type instance Dimension (SubLine d p s r) = d


deriving instance (Show r, Show s, Show p, Arity d) => Show (SubLine d p s r)
-- deriving instance (Read r, Read p, Arity d) => Read (SubLine d p r)
deriving instance (Eq r, Eq s, Fractional r, Eq p, Arity d)     => Eq (SubLine d p s r)
deriving instance Arity d                   => Functor (SubLine d p s)
deriving instance Arity d                   => F.Foldable (SubLine d p s)
deriving instance Arity d                   => T.Traversable (SubLine d p s)

instance (Arbitrary r, Arbitrary p, Arbitrary s, Arity d, Ord r, Ord s, Ord p, Num r)
         => Arbitrary (SubLine d p s r) where
  arbitrary = SubLine <$> arbitrary <*> arbitrary

-- | Annotate the subRange with the actual ending points
fixEndPoints    :: (Num r, Arity d) => SubLine d p r r -> SubLine d (Point d r :+ p) r r
fixEndPoints sl = sl&subRange %~ f
  where
    ptAt              = flip pointAt (sl^.line)
    label (c :+ e)    = (c :+ (ptAt c :+ e))
    f ~(Interval l u) = Interval (l&unEndPoint %~ label)
                                 (u&unEndPoint %~ label)

-- | forget the extra information stored at the endpoints of the subline.
dropExtra :: SubLine d p s r -> SubLine d () s r
dropExtra = over subRange (first (const ()))

_unBounded :: Prism' (SubLine d p (UnBounded r) r) (SubLine d p r r)
_unBounded = prism' toUnbounded fromUnbounded

-- | Transform into an subline with a potentially unbounded interval
toUnbounded :: SubLine d p r r -> SubLine d p (UnBounded r) r
toUnbounded = over subRange (fmap Val)

-- | Try to make a potentially unbounded subline into a bounded one.
fromUnbounded               :: SubLine d p (UnBounded r) r -> Maybe (SubLine d p r r)
fromUnbounded (SubLine l i) = SubLine l <$> mapM unBoundedToMaybe i

-- | given point p, and a Subline l r such that p lies on line l, test if it
-- lies on the subline, i.e. in the interval r
onSubLine                 :: (Ord r, Fractional r, Arity d)
                          => Point d r -> SubLine d p r r -> Bool
onSubLine p (SubLine l r) = case toOffset p l of
                              Nothing -> False
                              Just x  -> x `inInterval` r

-- | given point p, and a Subline l r such that p lies on line l, test if it
-- lies on the subline, i.e. in the interval r
onSubLineUB                   :: (Ord r, Fractional r)
                              => Point 2 r -> SubLine 2 p (UnBounded r) r -> Bool
p `onSubLineUB` (SubLine l r) = case toOffset p l of
                                  Nothing -> False
                                  Just x  -> Val x `inInterval` r

-- | given point p, and a Subline l r such that p lies on line l, test if it
-- lies on the subline, i.e. in the interval r
onSubLine2        :: (Ord r, Num r) => Point 2 r -> SubLine 2 p r r -> Bool
p `onSubLine2` sl = d `inInterval` r
  where
    -- get the endpoints (a,b) of the subline
    SubLine _ (Interval s e) = fixEndPoints sl
    a = s^.unEndPoint.extra.core
    b = e^.unEndPoint.extra.core
    d = (p .-. a) `dot` (b .-. a)
    -- map to an interval corresponding to the length of the segment
    r = Interval (s&unEndPoint.core .~ 0) (e&unEndPoint.core .~ squaredEuclideanDist b a)


-- | given point p, and a Subline l r such that p lies on line l, test if it
-- lies on the subline, i.e. in the interval r
onSubLine2UB        :: (Ord r, Fractional r)
                    => Point 2 r -> SubLine 2 p (UnBounded r) r -> Bool
p `onSubLine2UB` sl = p `onSubLineUB` sl


type instance IntersectionOf (SubLine 2 p s r) (SubLine 2 q s r) = [ NoIntersection
                                                                   , Point 2 r
                                                                   , SubLine 2 p s r
                                                                   ]

instance (Ord r, Fractional r) =>
         (SubLine 2 p r r) `IsIntersectableWith` (SubLine 2 p r r) where

  nonEmptyIntersection = defaultNonEmptyIntersection

  sl@(SubLine l r) `intersect` sm@(SubLine m _) = match (l `intersect` m) $
         (H $ \NoIntersection -> coRec NoIntersection)
      :& (H $ \p@(Point _)    -> if onSubLine2 p sl && onSubLine2 p sm
                                 then coRec p
                                 else coRec NoIntersection)
      :& (H $ \_             -> match (r `intersect` s'') $
                                      (H $ \NoIntersection -> coRec NoIntersection)
                                   :& (H $ \i              -> coRec $ SubLine l i)
                                   :& RNil
           )
      :& RNil
    where
      s'  = (fixEndPoints sm)^.subRange
      s'' = asProperInterval . first (^.extra)
          $ s'&start.core .~ toOffset' (s'^.start.extra.core) l
              &end.core   .~ toOffset' (s'^.end.extra.core)   l

instance (Ord r, Fractional r) =>
         (SubLine 2 p (UnBounded r) r) `IsIntersectableWith` (SubLine 2 p (UnBounded r) r) where
  nonEmptyIntersection = defaultNonEmptyIntersection

  sl@(SubLine l r) `intersect` sm@(SubLine m _) = match (l `intersect` m) $
         (H $ \NoIntersection -> coRec NoIntersection)
      :& (H $ \p@(Point _)    -> if onSubLine2UB p sl && onSubLine2UB p sm
                                 then coRec p
                                 else coRec NoIntersection)
      :& (H $ \_             -> match (r `intersect` s'') $
                                      (H $ \NoIntersection -> coRec NoIntersection)
                                   :& (H $ \i              -> coRec $ SubLine l i)
                                   :& RNil
           )
      :& RNil
    where
      -- convert to points, then convert back to 'r' values (but now w.r.t. l)
      s'  = getEndPointsUnBounded sm
      s'' = second (fmap f) s'
      f = flip toOffset' l

-- | Get the endpoints of an unbounded interval
getEndPointsUnBounded    :: (Num r, Arity d) => SubLine d p (UnBounded r) r
                         -> Interval p (UnBounded (Point d r))
getEndPointsUnBounded sl = second (fmap f) $ sl^.subRange
  where
    f = flip pointAt (sl^.line)

fromLine   :: Arity d => Line d r -> SubLine d () (UnBounded r) r
fromLine l = SubLine l (ClosedInterval (ext MinInfinity) (ext MaxInfinity))


-- testL :: SubLine 2 () (UnBounded Rational)
-- testL = SubLine (horizontalLine 0) (Interval (Closed (only 0)) (Open $ only 10))

-- horL :: SubLine 2 () (UnBounded Rational)
-- horL = fromLine $ horizontalLine 0


-- test = (testL^.subRange) `intersect` (horL^.subRange)

-- toOffset (Point2 minInfinity minInfinity) (horizontalLine 0)
-- testzz = let f  = bimap (fmap Val) (const ())
--          in

testz :: SubLine 2 () Rational Rational
testz = SubLine (Line (Point2 0 0) (Vector2 10 0))
                (Interval (Closed (0 % 1 :+ ())) (Closed (1 % 1 :+ ())))
