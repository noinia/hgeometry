{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.HalfLine
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Data.Geometry.HalfLine( HalfLine(HalfLine)
                             , startPoint, halfLineDirection
                             , toHalfLine
                             , halfLineToSubLine, fromSubLine
                             ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Boundary
import           Data.Geometry.Box
import           Data.Geometry.Interval
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.SubLine
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import qualified Data.Traversable as T
import           Data.UnBounded
import qualified Data.Vector.Fixed as FV
import           Data.Vinyl
import           Data.Vinyl.CoRec
import           GHC.Generics (Generic)
import           GHC.TypeLits

--------------------------------------------------------------------------------
-- * \(d\)-dimensional Half-Lines

-- | \(d\)-dimensional Half-Lines
data HalfLine d r = HalfLine { _startPoint        :: Point  d r
                             , _halfLineDirection :: Vector d r
                             } deriving Generic
makeLenses ''HalfLine

deriving instance (Show r, Arity d)   => Show    (HalfLine d r)
deriving instance (NFData r, Arity d) => NFData  (HalfLine d r)

deriving instance Arity d           => Functor (HalfLine d)
deriving instance Arity d           => F.Foldable    (HalfLine d)
deriving instance Arity d           => T.Traversable (HalfLine d)

type instance Dimension (HalfLine d r) = d
type instance NumType   (HalfLine d r) = r


instance {-# OVERLAPPING #-} (Eq r, Fractional r) => Eq (HalfLine 2 r) where
  (HalfLine p u) == (HalfLine q v) =
      p == q && -- Same starting point.
      isCoLinear p (Point u) (Point v) && -- Directions are on the same line.
      sameSigns -- Directions point in the same quadrant.
    where
      sameSigns = F.and $ FV.zipWith (\a b -> signum a==signum b) u v

instance (Eq r, Fractional r, Arity d) => Eq (HalfLine d r) where
  (HalfLine p u) == (HalfLine q v) = let lam = scalarMultiple u v
                                     in p == q && (signum <$> lam) == Just 1

instance HasStart (HalfLine d r) where
  type StartCore  (HalfLine d r) = Point d r
  type StartExtra (HalfLine d r) = ()

  start = lens ((:+ ()) . _startPoint) (\(HalfLine _ v) p -> HalfLine (p^.core) v)

instance HasSupportingLine (HalfLine d r) where
  supportingLine ~(HalfLine p v) = Line p v

-- Half-Lines are transformable
instance (Fractional r, Arity d, Arity (d + 1)) => IsTransformable (HalfLine d r) where
  transformBy t = toHalfLine . transformPointFunctor t . toLineSegment'
    where
      toLineSegment' :: (Num r, Arity d) => HalfLine d r -> LineSegment d () r
      toLineSegment' (HalfLine p v) = ClosedLineSegment (p :+ ()) ((p .+^ v) :+ ())

--------------------------------------------------------------------------------

halfLineToSubLine                :: (Arity d, Num r)
                                 => HalfLine d r -> SubLine d () (UnBounded r) r
halfLineToSubLine (HalfLine p v) = let l = Line p v
                                   in SubLine l (Interval (Closed $ ext (Val 0))
                                                          (Open   $ ext MaxInfinity))


fromSubLine               :: (Num r, Arity d) => SubLine d p (UnBounded r) r
                          -> Maybe (HalfLine d r)
fromSubLine (SubLine l i) = case (i^.start.core, i^.end.core) of
   (Val x, MaxInfinity) -> Just $ HalfLine (pointAt x l) (l^.direction)
   (MinInfinity, Val x) -> Just $ HalfLine (pointAt x l) ((-1) *^ l^.direction)
   _                    -> Nothing

type instance IntersectionOf (HalfLine d r) (Line d r) = [ NoIntersection
                                                         , Point d r
                                                         , HalfLine d r
                                                         ]

type instance IntersectionOf (HalfLine 2 r) (HalfLine 2 r) = [ NoIntersection
                                                             , Point 2 r
                                                             , LineSegment 2 () r
                                                             , HalfLine 2 r
                                                             ]

type instance IntersectionOf (LineSegment 2 p r) (HalfLine 2 r) = [ NoIntersection
                                                                  , Point 2 r
                                                                  , LineSegment 2 () r
                                                                  ]

type instance IntersectionOf (Point d r) (HalfLine d r) = [ NoIntersection
                                                          , Point d r
                                                          ]

instance (Ord r, Fractional r) => HalfLine 2 r `HasIntersectionWith` Line 2 r

instance (Ord r, Fractional r) => HalfLine 2 r `IsIntersectableWith` Line 2 r where
  nonEmptyIntersection = defaultNonEmptyIntersection
  hl `intersect` l = match (supportingLine hl `intersect` l) $
       H (\NoIntersection -> coRec NoIntersection)
    :& H (\p              -> if onHalfLine p hl then coRec p else coRec NoIntersection)
    :& H (\_l'            -> coRec hl)
    :& RNil


instance (Ord r, Fractional r) => HalfLine 2 r `HasIntersectionWith` HalfLine 2 r

instance (Ord r, Fractional r) => HalfLine 2 r `IsIntersectableWith` HalfLine 2 r where
  nonEmptyIntersection = defaultNonEmptyIntersection
  la@(HalfLine a va) `intersect` lb@(HalfLine b vb) =
    match (supportingLine la `intersect` supportingLine lb) $
         H (\NoIntersection -> coRec NoIntersection)
      :& H (\p              -> if onHalfLine p la && onHalfLine p lb
                               then coRec p else coRec NoIntersection)
      :& H (\_line          -> case ( a `onHalfLine ` lb
                                    , b `onHalfLine ` la
                                    , va `sameDirection` vb
                                    ) of
                                 (False,False,_)   -> coRec NoIntersection
                                 (True,True,True)  -> coRec la -- exact same halfline!
                                 (True,True,False) -> coRec $ ClosedLineSegment (ext a) (ext b)
                                 (True,_,True)     -> coRec la
                                 (_,True,True)     -> coRec lb
                                 (_,_,False)       -> error "HalfLine x Halfline intersection: impossible"
                                   -- it is impossible for a to be on
                                   -- lb, while b does not lie on la, while having different
                                   -- orientations

           )
      :& RNil

instance (Ord r, Fractional r) => LineSegment 2 () r `HasIntersectionWith` HalfLine 2 r

instance (Ord r, Fractional r) => LineSegment 2 () r `IsIntersectableWith` HalfLine 2 r where
  nonEmptyIntersection = defaultNonEmptyIntersection

  seg@(LineSegment s t) `intersect` hl@(HalfLine o _) =
    match (supportingLine seg `intersect` supportingLine hl) $
          H (\NoIntersection -> coRec NoIntersection)
      :&  H (\p              -> if onHalfLine p hl && p `intersects` seg then coRec p
                                                                         else coRec NoIntersection
            )
      :& H (\_line           -> case (o `intersects` seg, onHalfLine (t^.unEndPoint.core) hl) of
                                  (False,False) -> coRec NoIntersection
                                  (False,True)  -> coRec seg
                                  (True,True)   -> coRec $ LineSegment (Closed $ ext o) t
                                  (True,False)  -> coRec $ LineSegment s (Closed $ ext o)
           )
      :& RNil


instance (Ord r, Fractional r, Arity d) => Point d r `HasIntersectionWith` HalfLine d r where
  intersects = onHalfLine

instance (Ord r, Fractional r, Arity d) => Point d r `IsIntersectableWith` HalfLine d r where
  nonEmptyIntersection = defaultNonEmptyIntersection
  p `intersect` hl | p `intersects` hl = coRec p
                   | otherwise         = coRec NoIntersection



type instance IntersectionOf (HalfLine 2 r) (Boundary (Rectangle p r)) =
  [ NoIntersection, Point 2 r, (Point 2 r, Point 2 r) , LineSegment 2 () r]

type instance IntersectionOf (HalfLine 2 r) (Rectangle p r) = [ NoIntersection
                                                              , Point 2 r
                                                              , LineSegment 2 () r
                                                              ]
instance (Ord r, Fractional r)
         => HalfLine 2 r `HasIntersectionWith` Boundary (Rectangle p r)

instance (Ord r, Fractional r)
         => HalfLine 2 r `IsIntersectableWith` Boundary (Rectangle p r) where
  nonEmptyIntersection = defaultNonEmptyIntersection

  hl@(HalfLine o v) `intersect` br = match (Line o v `intersect` br) $
       H coRec -- NoIntersection
    :& H (\p -> if p `intersects` hl then coRec p else coRec NoIntersection)
    :& H (\(p,q) -> case (p `intersects` hl, q `intersects` hl) of
                      (False,False) -> coRec NoIntersection
                      (False,True)  -> coRec q
                      (True,False)  -> coRec p
                      (True,True)   -> coRec (p,q))
    :& H (\s@(LineSegment' p q) -> case ((p^.core) `intersects` hl, (q^.core) `intersects` hl) of
                      (False,False) -> coRec NoIntersection
                      (False,True)  -> coRec $ ClosedLineSegment (ext o) q
                      (True,False)  -> coRec $ ClosedLineSegment (ext o) p
                      (True,True)   -> coRec s)
    :& RNil
instance (Ord r, Fractional r)
         => HalfLine 2 r `HasIntersectionWith` Rectangle p r

instance (Ord r, Fractional r)
         => HalfLine 2 r `IsIntersectableWith` Rectangle p r where
  nonEmptyIntersection = defaultNonEmptyIntersection

  hl@(HalfLine o _) `intersect` rect  = match (hl `intersect` Boundary rect) $
       H coRec -- NoIntersection
    :& H (\p -> if o `insideBox` rect then coRec (ClosedLineSegment (ext o) (ext p))
                                      else coRec p -- p is on the boundary
         )
    :& H (\(p,q) -> coRec $ ClosedLineSegment (ext p) (ext q))
    :& H coRec -- LineSegment
    :& RNil

-- | Test if a point lies on a half-line
onHalfLine :: (Ord r, Fractional r, Arity d) => Point d r -> HalfLine d r -> Bool
p `onHalfLine` (HalfLine q v) = maybe False (>= 0) $ scalarMultiple (p .-. q) v



-- | Transform a LineSegment into a half-line, by forgetting the second endpoint.
-- Note that this also forgets about if the starting point was open or closed.
toHalfLine   :: (Num r, Arity d) => LineSegment d p r -> HalfLine d r
toHalfLine s = let p = s^.start.core
                   q = s^.end.core
               in HalfLine p (q .-. p)
