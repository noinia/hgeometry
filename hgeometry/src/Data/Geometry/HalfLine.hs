{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module Data.Geometry.HalfLine where


import           Control.DeepSeq
import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
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

type instance IntersectionOf (HalfLine 2 r) (Line 2 r) = [ NoIntersection
                                                         , Point 2 r
                                                         , HalfLine 2 r
                                                         ]

type instance IntersectionOf (HalfLine 2 r) (HalfLine 2 r) = [ NoIntersection
                                                             , Point 2 r
                                                             , LineSegment 2 () r
                                                             , HalfLine 2 r
                                                             ]

type instance IntersectionOf (HalfLine 2 r) (LineSegment 2 p r) = [ NoIntersection
                                                                  , Point 2 r
                                                                  , LineSegment 2 () r
                                                                  ]


-- instance (Ord r, Fractional r) => (HalfLine 2 r) `IsIntersectableWith` (Line 2 r) where
  -- hl `intersect` l = match (halfLineToSubLine hl, l)


-- instance (Ord r, Fractional r) => (HalfLine 2 r) `IsIntersectableWith` (Line 2 r) where
--   data Intersection (HalfLine 2 r) (Line 2 r) = NoHalfLineLineIntersection
--                                               | HalfLineLineIntersection !(Point 2 r)
--                                               | HalfLineLineOverlap      !(HalfLine 2 r)
--                                               deriving (Show,Eq)

--   nonEmptyIntersection NoHalfLineLineIntersection = False
--   nonEmptyIntersection _                          = True

--   hl `intersect` l = case supportingLine hl `intersect` l of
--     SameLine _             -> HalfLineLineOverlap hl
--     LineLineIntersection p -> if p `onHalfLine` hl then HalfLineLineIntersection p
--                                                    else NoHalfLineLineIntersection
--     ParallelLines          -> NoHalfLineLineIntersection


-- instance (Ord r, Fractional r) => (HalfLine 2 r) `IsIntersectableWith` (HalfLine 2 r) where
--   data Intersection (HalfLine 2 r) (HalfLine 2 r) = NoHalfLineHalfLineIntersection
--                                                   | HLHLIntersectInPoint    !(Point 2 r)
--                                                   | HLHLIntersectInSegment  !(LineSegment 2 () r)
--                                                   | HLHLIntersectInHalfLine !(HalfLine 2 r)
--                                                   deriving (Show,Eq)

--   nonEmptyIntersection NoHalfLineHalfLineIntersection = False
--   nonEmptyIntersection _                              = True

--   hl' `intersect` hl = case supportingLine hl' `intersect` supportingLine hl of
--     ParallelLines          -> NoHalfLineHalfLineIntersection
--     LineLineIntersection p -> if p `onHalfLine` hl' && p `onHalfLine` hl then HLHLIntersectInPoint p
--                                                                          else NoHalfLineHalfLineIntersection
--     SameLine _             -> let p   = _startPoint hl'
--                                   q   = _startPoint hl
--                                   seg = LineSegment (p :+ ()) (q :+ ())
--                               in case (p `onHalfLine` hl, q `onHalfLine` hl') of
--                                    (False,False) -> NoHalfLineHalfLineIntersection
--                                    (False,True)  -> HLHLIntersectInHalfLine hl
--                                    (True, False) -> HLHLIntersectInHalfLine hl'
--                                    (True, True)  -> if hl == hl' then HLHLIntersectInHalfLine hl
--                                                                  else HLHLIntersectInSegment seg



-- instance (Ord r, Fractional r) => (LineSegment 2 p r) `IsIntersectableWith` (HalfLine 2 r) where
--   data Intersection (LineSegment 2 p r) (HalfLine 2 r) = NoSegmentHalfLineIntersection
--                                                        | SegmentHalfLineIntersection !(Point 2 r)
--                                                        | SegmentOnHalfLine           !(LineSegment 2 () r)

--   nonEmptyIntersection NoSegmentHalfLineIntersection = False
--   nonEmptyIntersection _                             = True

--   s `intersect` hl = case supportingLine s `intersect` supportingLine hl of
--     ParallelLines          -> NoSegmentHalfLineIntersection
--     LineLineIntersection p -> if p `onSegment` s && p `onHalfLine` hl then SegmentHalfLineIntersection p
--                                                                       else NoSegmentHalfLineIntersection
--     SameLine _             -> let p = s  ^.start.core
--                                   q = s  ^.end.core
--                                   r = hl ^.start.core
--                                   seg a b = LineSegment (a :+ ()) (b :+ ())
--                               in case (p `onHalfLine` hl, q `onHalfLine` hl) of
--                                    (False, False)   -> NoSegmentHalfLineIntersection
--                                    (False, True)    -> SegmentOnHalfLine $ seg r q
--                                    (True,  False)   -> SegmentOnHalfLine $ seg p r
--                                    (True,  True)    -> SegmentOnHalfLine $ seg p q




-- | Test if a point lies on a half-line
onHalfLine :: (Ord r, Fractional r, Arity d) => Point d r -> HalfLine d r -> Bool
p `onHalfLine` (HalfLine q v) = maybe False (>= 0) $ scalarMultiple (p .-. q) v





-- | Transform a LineSegment into a half-line, by forgetting the second endpoint.
-- Note that this also forgets about if the starting point was open or closed.
toHalfLine   :: (Num r, Arity d) => LineSegment d p r -> HalfLine d r
toHalfLine s = let p = s^.start.core
                   q = s^.end.core
               in HalfLine p (q .-. p)
