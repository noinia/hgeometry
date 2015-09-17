{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Geometry.PolyLine where

import           Control.Applicative
import           Control.Lens
import           Data.Bifunctor
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Box
import           Data.Geometry.Interval
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import qualified Data.Seq2 as S2
import qualified Data.Sequence as Seq
import           Data.Semigroup

--------------------------------------------------------------------------------
-- * d-dimensional Polygonal Lines (PolyLines)

-- | A Poly line in R^d
newtype PolyLine d p r = PolyLine { _points :: S2.Seq2 (Point d r :+ p) }
makeLenses ''PolyLine

deriving instance (Show r, Show p, Arity d) => Show    (PolyLine d p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq      (PolyLine d p r)
deriving instance (Ord r, Ord p, Arity d)   => Ord     (PolyLine d p r)

instance Arity d => Functor (PolyLine d p) where
  fmap f (PolyLine ps) = PolyLine $ fmap (first (fmap f)) ps

type instance Dimension (PolyLine d p r) = d
type instance NumType   (PolyLine d p r) = r

instance Semigroup (PolyLine d p r) where
  (PolyLine pts) <> (PolyLine pts') = PolyLine $ pts <> pts'

instance Arity d => IsBoxable (PolyLine d p r) where
  boundingBox = boundingBoxList . toListOf (points.traverse.core)

instance (Num r, AlwaysTruePFT d) => IsTransformable (PolyLine d p r) where
  transformBy = transformPointFunctor

instance PointFunctor (PolyLine d p) where
  pmap f = over points (fmap (first f))

instance Arity d => Bifunctor (PolyLine d) where
  bimap f g (PolyLine pts) = PolyLine $ fmap (bimap (fmap g) f) pts


-- | pre: The input list contains at least two points
fromPoints :: [Point d r :+ p] -> PolyLine d p r
fromPoints = PolyLine . S2.fromList

-- | pre: The input list contains at least two points. All extra vields are
-- initialized with mempty.
fromPoints' :: (Monoid p) => [Point d r] -> PolyLine d p r
fromPoints' = fromPoints . map (\p -> p :+ mempty)


-- | We consider the line-segment as closed.
fromLineSegment                    :: LineSegment d p r -> PolyLine d p r
fromLineSegment (LineSegment' p q) = fromPoints [p,q]

-- | Convert to a closed line segment by taking the first two points.
asLineSegment                              :: PolyLine d p r -> LineSegment d p r
asLineSegment (PolyLine (S2.Seq2 p mid q)) = ClosedLineSegment p (f $ Seq.viewl mid)
  where
    f Seq.EmptyL    = q
    f (q' Seq.:< _) = q'

-- | Stricter version of asLineSegment that fails if the Polyline contains more
-- than two points.
asLineSegment'                            :: PolyLine d p r -> Maybe (LineSegment d p r)
asLineSegment' (PolyLine (S2.Seq2 p m q))
  | Seq.null m                            = Just $ ClosedLineSegment p q
  | otherwise                             = Nothing
