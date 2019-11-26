{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
module Data.Geometry.Slab where

import           Control.Lens (makeLenses, (^.),(%~),(.~),(&), both, from)
import           Data.Bifunctor
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Box.Internal
import           Data.Geometry.Interval
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.SubLine
import qualified Data.Traversable as T
import           Data.Vinyl
import           Data.Vinyl.CoRec

--------------------------------------------------------------------------------

data Orthogonal = Horizontal | Vertical
                deriving (Show,Eq,Read)



newtype Slab (o :: Orthogonal) a r = Slab { _unSlab :: Interval a r }
                                     deriving (Show,Eq)
makeLenses ''Slab

-- | Smart consturctor for creating a horizontal slab
horizontalSlab     :: (r :+ a) -> (r :+ a) -> Slab Horizontal a r
horizontalSlab l h = Slab $ ClosedInterval l h

-- | Smart consturctor for creating a vertical slab
verticalSlab :: (r :+ a) -> (r :+ a) -> Slab Vertical a r
verticalSlab l r = Slab $ ClosedInterval l r

instance Functor (Slab o a) where
  fmap = T.fmapDefault

instance F.Foldable (Slab o a) where
  foldMap = T.foldMapDefault

instance T.Traversable (Slab o a) where
  traverse f (Slab i) = Slab <$> T.traverse f i


instance Bifunctor (Slab o) where
  bimap f g (Slab i) = Slab $ bimap f g i


type instance IntersectionOf (Slab o a r)          (Slab o a r) =
  [NoIntersection, Slab o a r]
type instance IntersectionOf (Slab Horizontal a r) (Slab Vertical a r) =
  '[Rectangle (a,a) r]


instance Ord r => (Slab o a r) `IsIntersectableWith` (Slab o a r) where
  nonEmptyIntersection = defaultNonEmptyIntersection

  (Slab i) `intersect` (Slab i') = match (i `intersect` i') $
        (H $ \NoIntersection -> coRec NoIntersection)
     :& (H $ \i''            -> coRec (Slab i'' :: Slab o a r))
     :& RNil

instance (Slab Horizontal a r) `IsIntersectableWith` (Slab Vertical a r) where
  nonEmptyIntersection _ _ _ = True

  (Slab h) `intersect` (Slab v) = coRec $ box low high
    where
      low  = Point2 (v^.start.core) (h^.start.core) :+ (v^.start.extra, h^.start.extra)
      high = Point2 (v^.end.core)   (h^.end.core)   :+ (v^.end.extra,   h^.end.extra)



class HasBoundingLines (o :: Orthogonal) where
  -- | The two bounding lines of the slab, first the lower one, then the higher one:
  --
  boundingLines :: Num r => Slab o a r -> (Line 2 r :+ a, Line 2 r :+ a)

  inSlab :: Ord r => Point 2 r -> Slab o a r -> Bool


instance HasBoundingLines Horizontal where
  boundingLines (Slab i) = (i^.start, i^.end)&both.core %~ horizontalLine

  p `inSlab` (Slab i) = (p^.yCoord) `inInterval` i


instance HasBoundingLines Vertical where
  boundingLines (Slab i) = (i^.start, i^.end)&both.core %~ verticalLine

  p `inSlab` (Slab i) = (p^.xCoord) `inInterval` i


type instance IntersectionOf (Line 2 r) (Slab o a r) =
  [NoIntersection, Line 2 r, LineSegment 2 a r]

instance (Fractional r, Ord r, HasBoundingLines o) =>
         Line 2 r `IsIntersectableWith` (Slab o a r) where
  nonEmptyIntersection = defaultNonEmptyIntersection

  l@(Line p _) `intersect` s = match (l `intersect` a) $
         (H $ \NoIntersection -> if p `inSlab` s then coRec l else coRec NoIntersection)
      :& (H $ \pa             -> match (l `intersect` b) $
            (H $ \NoIntersection -> coRec NoIntersection)
         :& (H $ \pb             -> coRec $ lineSegment' pa pb)
         :& (H $ \_              -> coRec l)
         :& RNil
         )
      :& (H $ \_              -> coRec l)
      :& RNil
    where
      (a :+ _,b :+ _) = boundingLines s

      -- note that this maintains the open/closedness of the slab
      lineSegment' pa pb = let Interval a' b' = s^.unSlab
                           in LineSegment (a'&unEndPoint.core .~ pa)
                                          (b'&unEndPoint.core .~ pb)



type instance IntersectionOf (SubLine 2 p s r) (Slab o a r) =
  [NoIntersection, SubLine 2 () s r]

instance (Fractional r, Ord r, HasBoundingLines o) =>
         SubLine 2 a r r `IsIntersectableWith` (Slab o a r) where

  nonEmptyIntersection = defaultNonEmptyIntersection

  sl@(SubLine l _) `intersect` s = match (l `intersect` s) $
       (H $ \NoIntersection -> coRec NoIntersection)
    :& (H $ \(Line _ _)     -> coRec $ dropExtra sl)
    :& (H $ \seg            -> match (sl `intersect` (seg^._SubLine)) $
                                    (H $ \NoIntersection -> coRec NoIntersection)
                                 :& (H $ \p@(Point2 _ _) -> coRec $ singleton p)
                                 :& (H $ \ss             -> coRec $ dropExtra ss)
                                 :& RNil)
    :& RNil
    where
      singleton p = let x = ext $ toOffset' p l in SubLine l (ClosedInterval x x)


type instance IntersectionOf (LineSegment 2 p r) (Slab o a r) =
  [NoIntersection, LineSegment 2 () r]

instance (Fractional r, Ord r, HasBoundingLines o) =>
         LineSegment 2 a r `IsIntersectableWith` (Slab o a r) where
  nonEmptyIntersection = defaultNonEmptyIntersection

  seg `intersect` slab = match ((seg^._SubLine) `intersect` slab) $
       (H $ \NoIntersection -> coRec   NoIntersection)
    :& (H $ \sl             -> coRec $ sl^. from _SubLine)
    :& RNil




-- test :: SubLine 2 () Double Double
-- test = (ClosedLineSegment (ext origin) (ext origin))^._SubLine
