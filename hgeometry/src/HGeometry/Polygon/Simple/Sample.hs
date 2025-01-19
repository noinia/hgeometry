--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Simple.Sample
-- Copyright   :  (C) Frank Staals, Owen Graves,  David Himmelstrup
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Functionality to sample points uniformly at random from within a simple polygon.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Simple.Sample
  ( samplePolygon
  , samplePolygons

  , Sampler
  , samplePoint
  , triangleSampler
  ) where

import           Control.Lens
import           Data.Foldable1
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           HGeometry.Ext
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Polygon
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Polygon.Triangulation
import           HGeometry.Triangle
import           HGeometry.Vector
import           System.Random.Stateful

--------------------------------------------------------------------------------

-- | A data structure that can be used to efficiently sample values of type v.
data Sampler w v = Sampler !w -- ^ the total weight
                           (Map.Map w v)
  deriving (Show,Read,Eq,Functor,Foldable)

instance Traversable (Sampler w) where
  traverse f (Sampler w m) = Sampler w <$> traverse f m

-- | Build a sampler
--
-- O(n)
buildSampler    :: (Foldable1 nonEmpty, Num w, Ord w) => nonEmpty (w, v) -> Sampler w v
buildSampler xs = let Weighted total xs'       = foldr f (Weighted 0 []) xs
                      f (w,x) (Weighted t acc) = Weighted (w+t) ((t,x):acc)
                  in Sampler total (Map.fromDescList xs')

-- | Helper data type
data Weighted w v = Weighted !w v deriving (Show)

-- | Sample a value from the sampler
--
-- \(O(\log n)\)
sample                     :: (StatefulGen g m, Ord w, Num w, UniformRange w)
                           => Sampler w v -> g -> m v
sample (Sampler total m) g = (maybe err snd . flip Map.lookupLE m) <$> uniformRM (0, total) g
  where err = error "sample: absurd."


--------------------------------------------------------------------------------


-- | Build a triangle sampler; i.e. samples a triagnle from the polygons
-- with probability proportional to its area.
--
-- \(O(N\log N)\), where \(N\) is the total size of all polygons.
triangleSampler :: (SimplePolygon_ polygon point r, Num r, Ord r, Foldable1 nonEmpty)
                => nonEmpty polygon -> Sampler r (Triangle point)
triangleSampler = buildSampler . fmap (\tri -> (triangleSignedArea2X tri, tri))
                . foldMap1 toTriangles

-- | Sample a point
samplePoint           :: (Point_ point 2 r, StatefulGen g m, Real r, Ord r, UniformRange r)
                      => Sampler r (Triangle point) -> g -> m (Point 2 Double)
samplePoint sampler g = sample sampler g >>= flip sampleTriangle g

-- | Unfiormly samples a point from a set of polygons. You may want to build a
-- pointSampler if the goal is to sample multiple points.
--
-- \(O(N\log N)\), where \(N\) is the total size of all polygons.
samplePolygons       :: ( SimplePolygon_ polygon point r, StatefulGen g m
                        , Foldable1 nonEmpty, Real r, Ord r, UniformRange r
                        )
                     => nonEmpty polygon -> g -> m (Point 2 Double)
samplePolygons pgs g = flip samplePoint g $ triangleSampler pgs

-- | Uniformly samples a point in a polygon.
--
-- \(O(n\log n)\)
samplePolygon   :: ( SimplePolygon_ polygon point r, Ord r, Real r, UniformRange r
                   , StatefulGen g m)
                => polygon -> g -> m (Point 2 Double)
samplePolygon p = samplePolygons $ p NonEmpty.:| []

-- | Uniformly samples a triangle in \(O(1)\) time.
sampleTriangle                                       :: ( Point_ point 2 r, Real r
                                                       , StatefulGen g m)
                                                     => Triangle point -> g -> m (Point 2 Double)
sampleTriangle (fmap doubleP -> Triangle v1 v2 v3) g =
    f <$> uniformRM (0, 1) g <*> uniformRM (0, 1) g
  where
    f       :: Double -> Double -> Point 2 Double
    f a' b' = let (a, b) = if a' + b' > 1 then (1 - a', 1 - b') else (a', b')
                  u = v2 .-. v1
                  v = v3 .-. v1
              in v1 .+^ (a*^u) .+^ (b*^v)
-- the idea is based on
-- https://blogs.sas.com/content/iml/2020/10/19/random-points-in-triangle.html

-- | Convert to a point with double coordiantes
doubleP :: (Point_ point 2 r, Real r) => point -> Point 2 Double
doubleP = over coordinates realToFrac . view asPoint

-- | Triangulates a polygon \(O(n \log n)\).
toTriangles :: (SimplePolygon_ polygon point r, Num r, Ord r)
            => polygon -> NonEmpty.NonEmpty (Triangle point)
toTriangles = NonEmpty.fromList . fmap (fmap (view core))
            . mapMaybe asTriangle . toListOf interiorFacePolygons . triangulate @()
  -- any valid simple polygon produces at least one triangle, so the
  -- NonEmpty.fromList is safe.
