{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Geometry.PolyLine where

import           Control.Lens
import           Data.Aeson
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Box
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import           Data.LSeq (LSeq, pattern (:<|))
import qualified Data.LSeq as LSeq
import qualified Data.List.NonEmpty as NE
import           GHC.Generics (Generic)
import           GHC.TypeLits

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- $setup
-- >>> :{
-- let myPolyLine = fromPointsUnsafe $ map ext [origin, Point2 10.0 10.0, Point2 10.0 20.0]
-- :}

--------------------------------------------------------------------------------
-- * d-dimensional Polygonal Lines (PolyLines)

-- | A Poly line in R^d has at least 2 vertices
newtype PolyLine d p r = PolyLine { _points :: LSeq 2 (Point d r :+ p) } deriving (Generic)
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
  boundingBox = boundingBoxList . NE.fromList . toListOf (points.traverse.core)

instance (Fractional r, Arity d, Arity (d + 1)) => IsTransformable (PolyLine d p r) where
  transformBy = transformPointFunctor

instance PointFunctor (PolyLine d p) where
  pmap f = over points (fmap (first f))

instance Arity d => Bifunctor (PolyLine d) where
  bimap = bimapDefault
instance Arity d => Bifoldable (PolyLine d) where
  bifoldMap = bifoldMapDefault
instance Arity d => Bitraversable (PolyLine d) where
  bitraverse f g (PolyLine pts) = PolyLine <$> traverse (bitraverse (traverse g) f) pts

instance (ToJSON p, ToJSON r, Arity d) => ToJSON (PolyLine d p r) where
    toEncoding = genericToEncoding defaultOptions
instance (FromJSON p, FromJSON r, Arity d, KnownNat d) => FromJSON (PolyLine d p r)

-- | Builds a Polyline from a list of points, if there are sufficiently many points
fromPoints :: [Point d r :+ p] -> Maybe (PolyLine d p r)
fromPoints = fmap PolyLine . LSeq.eval (C @ 2) . LSeq.fromList

-- | pre: The input list contains at least two points
fromPointsUnsafe :: [Point d r :+ p] -> PolyLine d p r
fromPointsUnsafe = PolyLine . LSeq.forceLSeq (C @ 2) . LSeq.fromList

-- | pre: The input list contains at least two points. All extra vields are
-- initialized with mempty.
fromPointsUnsafe' :: (Monoid p) => [Point d r] -> PolyLine d p r
fromPointsUnsafe' = fromPointsUnsafe . map (\p -> p :+ mempty)


-- | We consider the line-segment as closed.
fromLineSegment                     :: LineSegment d p r -> PolyLine d p r
fromLineSegment ~(LineSegment' p q) = fromPointsUnsafe [p,q]

-- | Convert to a closed line segment by taking the first two points.
asLineSegment                            :: PolyLine d p r -> LineSegment d p r
asLineSegment (PolyLine (p :<| q :<| _)) = ClosedLineSegment p q

-- | Stricter version of asLineSegment that fails if the Polyline contains more
-- than two points.
asLineSegment'                :: PolyLine d p r -> Maybe (LineSegment d p r)
asLineSegment' (PolyLine pts) = case F.toList pts of
                                  [p,q] -> Just $ ClosedLineSegment p q
                                  _     -> Nothing

-- | Computes the edges, as linesegments, of an LSeq
edgeSegments    :: Arity d => PolyLine d p r -> LSeq 1 (LineSegment d p r)
edgeSegments pl = let vs = pl^.points
                  in LSeq.zipWith ClosedLineSegment (LSeq.init vs) (LSeq.tail vs)


-- | Linearly interpolate the polyline with a value in the range
-- \([0,n-1]\), where \(n\) is the number of vertices of the polyline.
--
-- running time: \(O(\log n)\)
--
-- >>> interpolatePoly 0.5 myPolyLine
-- Point2 [5.0,5.0]
-- >>> interpolatePoly 1.5 myPolyLine
-- Point2 [10.0,15.0]
interpolatePoly      :: (RealFrac r, Arity d) => r -> PolyLine d p r -> Point d r
interpolatePoly t pl = let i = floor t in case edgeSegments pl^?ix i of
                         Nothing -> pl^.points.to LSeq.last.core
                         Just e  -> interpolate (t-fromIntegral i) e
