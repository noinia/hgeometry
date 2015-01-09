{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE TemplateHaskell  #-}
module Data.Geometry.PolyLine where

import           Control.Applicative
import           Control.Lens
import qualified Data.Foldable as F

import           Data.Monoid

import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import           Linear.Affine(Affine(..))


-- import           Data.Sequence((<|))
import qualified Data.Sequence as S

import           GHC.TypeLits

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


instance (Eq r, Fractional r) => (Line 2 r) `IsIntersectableWith` (Line 2 r) where
  data Intersection (Line 2 r) (Line 2 r) = SameLine             (Line 2 r)
                                          | LineLineIntersection (Point 2 r)
                                          | ParallelLines -- ^ No intersection
                                            deriving (Show)
  l@(Line p u) `intersect` m@(Line q v)
      | l `isParallelTo` m = if q `onLine` l then SameLine l else ParallelLines
      | otherwise          = LineLineIntersection r
    where
      r = undefined -- TODO: Finish this



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
data LineSegment d p r = LineSegment { _start :: Point d r :+ p
                                     , _end   :: Point d r :+ p
                                     }
makeLenses ''LineSegment

deriving instance (Show r, Show p, Arity d) => Show (LineSegment d p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq (LineSegment d p r)
deriving instance (Ord r, Ord p, Arity d)   => Ord (LineSegment d p r)
deriving instance Arity d                   => Functor (LineSegment d p)
type instance Dimension (LineSegment d p r) = d
type instance NumType   (LineSegment d p r) = r

instance HasPoints (LineSegment d p r) where
  points l = [l^.start.core, l^.end.core]

instance PointFunctor (LineSegment d p) where
  pmap f (LineSegment s e) = LineSegment (f <$> s) (f <$> e)

instance Arity d => IsBoxable (LineSegment d p r) where
  boundingBox = boundingBoxList . points

instance (Num r, AlwaysTruePFT d) => IsTransformable (LineSegment d p r) where
  transformBy = transformPointFunctor



toLineSegment            :: (Monoid p, Num r, Arity d) => Line d r -> LineSegment d p r
toLineSegment (Line p v) = LineSegment (p       :+ mempty)
                                       (p .+^ v :+ mempty)

toLine                                 :: (Num r, Arity d) => LineSegment d p r -> Line d r
toLine (LineSegment (p :+ _) (q :+ _)) = lineThrough p q





-- | Lines are transformable, via line segments
instance (Num r, AlwaysTruePFT d) => IsTransformable (Line d r) where
  transformBy t = toLine . transformPointFunctor t . toLineSegment'
    where
      toLineSegment' :: (Num r, Arity d) => Line d r -> LineSegment d () r
      toLineSegment' = toLineSegment

--------------------------------------------------------------------------------

-- | A Poly line in R^d
newtype PolyLine d p r = PolyLine { _unPolyLine :: S.Seq (Point d r :+ p) }
makeLenses ''PolyLine

deriving instance (Show r, Show p, Arity d) => Show    (PolyLine d p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq      (PolyLine d p r)
deriving instance (Ord r, Ord p, Arity d)   => Ord     (PolyLine d p r)
deriving instance Arity d                   => Functor (PolyLine d p)
type instance Dimension (PolyLine d p r) = d
type instance NumType   (PolyLine d p r) = r

instance HasPoints (PolyLine d p r) where
  points = F.toList . fmap _core . _unPolyLine

instance Arity d => IsBoxable (PolyLine d p r) where
  boundingBox = boundingBoxList . points

instance (Num r, AlwaysTruePFT d) => IsTransformable (PolyLine d p r) where
  transformBy = transformPointFunctor

instance PointFunctor (PolyLine d p) where
  pmap f = over unPolyLine (fmap (fmap f))

fromPoints :: (Monoid p, F.Foldable f) => f (Point 2 r) -> PolyLine 2 p r
fromPoints = PolyLine . F.foldr (\p s -> (p :+ mempty) <| s) S.empty
