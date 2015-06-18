{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.HalfLine where

import           Control.Applicative
import           Control.Lens hiding (only)
import           Data.Ext
import           Data.Range
import           Data.Geometry.Interval
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import           Data.Geometry.Line
import           Data.Geometry.SubLine
import           Data.Geometry.LineSegment
import           Linear.Vector((*^))
import           Linear.Affine(Affine(..),distanceA)
import qualified Data.Traversable as T
import qualified Data.Foldable as F


--------------------------------------------------------------------------------
-- * d-dimensional Half-Lines

-- | d-dimensional Half-Lines
data HalfLine d r = HalfLine { _startPoint        :: Point  d r
                             , _halfLineDirection :: Vector d r
                             }
makeLenses ''HalfLine

deriving instance (Show r, Arity d) => Show    (HalfLine d r)
deriving instance (Eq r, Arity d)   => Eq      (HalfLine d r)
deriving instance Arity d           => Functor (HalfLine d)
deriving instance Arity d           => F.Foldable    (HalfLine d)
deriving instance Arity d           => T.Traversable (HalfLine d)

type instance Dimension (HalfLine d r) = d
type instance NumType   (HalfLine d r) = r

instance HasStart (HalfLine d r) where
  type StartCore  (HalfLine d r) = Point d r
  type StartExtra (HalfLine d r) = ()

  start = lens ((:+ ()) . _startPoint) (\(HalfLine _ v) p -> HalfLine (p^.core) v)

instance HasSupportingLine (HalfLine d r) where
  supportingLine ~(HalfLine p v) = Line p v

-- Half-Lines are transformable
instance (Num r, AlwaysTruePFT d) => IsTransformable (HalfLine d r) where
  transformBy t = toHalfLine . transformPointFunctor t . toLineSegment'
    where
      toLineSegment' :: (Num r, Arity d) => HalfLine d r -> LineSegment d () r
      toLineSegment' (HalfLine p v) = LineSegment (p :+ ()) ((p .+^ v) :+ ())

--------------------------------------------------------------------------------

halfLineToSubLine                :: (Arity d, Num r) => HalfLine d r -> SubLine d () (Top r)
halfLineToSubLine (HalfLine p v) = let l = fmap Val $ Line p v
                                   in SubLine l (Interval $ Range (Closed $ only (Val 0))
                                                                  (Open   $ only Top))


fromSubLine               :: (Num r, Arity d) => SubLine d p (Top r) -> Maybe (HalfLine d r)
fromSubLine (SubLine l' i) = toMaybe $ do
                              l@(Line _ v) <- T.sequence l'
                              let ts = i^.start.core
                                  te = i^.end.core
                              case (ts,te) of
                                (Top,Top)  -> Top
                                (Val x,_)  -> Val $ HalfLine (pointAt x l) v
                                (_, Val x) -> Val $ HalfLine (pointAt x l) v


type instance IntersectionOf (HalfLine 2 r) (Line 2 r) = [ NoIntersection
                                                         , Point 2 r
                                                         , HalfLine 2 r
                                                         ]

type instance IntersectionOf (HalfLine 2 r) (HalfLine 2 r) = [ NoIntersection
                                                             , Point 2 r
                                                             , LineSegment 2 () r
                                                             , HalfLine 2 r
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
toHalfLine                     :: (Num r, Arity d) => LineSegment d p r -> HalfLine d r
toHalfLine (LineSegment p' q') = let p = p' ^.core
                                     q = q' ^.core
                                 in HalfLine p (q .-. p)
