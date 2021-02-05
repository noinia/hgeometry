{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
module Algorithms.Geometry.PointLocation.PersistentSweep
  ( PointLocationDS(PointLocationDS)
  , verticalRayShootingStructure, subdivision, outerFace

  -- * Building the Data Structure
  , pointLocationDS
  -- * Querying the Data Structure
  , dartAbove, dartAboveOrOn
  , faceContaining, faceIdContaining

  , InPolygonDS, inPolygonDS
  , InOut(..)

  , pointInPolygon
  , edgeOnOrAbove
  ) where

import qualified Algorithms.Geometry.VerticalRayShooting.PersistentSweep as VRS
import           Control.Lens hiding (contains, below)
import           Data.Ext
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Proxy
import qualified Data.Vector as V

--------------------------------------------------------------------------------

-- | Planar Point Location Data structure
data PointLocationDS s v e f r = PointLocationDS {
        _verticalRayShootingStructure :: VRS.VerticalRayShootingStructure v (Dart s) r
      , _subdivision                  :: PlanarSubdivision s v e f r
      , _outerFace                    :: FaceId' s
      } deriving (Show,Eq)

makeLensesWith (lensRules&generateUpdateableOptics .~ False) ''PointLocationDS

--------------------------------------------------------------------------------
-- * Buidlding the Point location Data structure

-- | Builds a pointlocation data structure on the planar subdivision with \(n\)
-- vertices.
--
-- running time: \(O(n\log n)\).
-- space: \(O(n\log n)\).
pointLocationDS    :: (Ord r, Fractional r)
                   => PlanarSubdivision s v e f r -> PointLocationDS s v e f r
pointLocationDS ps = PointLocationDS (VRS.verticalRayShootingStructure es) ps (outerFaceId ps)
  where
    es = NonEmpty.fromList . V.toList . fmap (\(d,s) -> s&extra .~ d) . edgeSegments $ ps
      -- the VRS structure will throw away vertical edges. So there is no need to
      -- explicitly filter them yet at this point

--------------------------------------------------------------------------------
-- * Querying the Structure

-- | Locates the first edge (dart) strictly above the query point.
-- returns Nothing if the query point lies in the outer face and there is no dart
-- above it.
--
-- running time: \(O(\log n)\)
dartAbove :: (Ord r, Fractional r)
          => Point 2 r -> PointLocationDS s v e f r -> Maybe (Dart s)
dartAbove = queryWith VRS.segmentAbove

dartAboveOrOn :: (Ord r, Fractional r)
              => Point 2 r -> PointLocationDS s v e f r -> Maybe (Dart s)
dartAboveOrOn = queryWith VRS.segmentAboveOrOn

type QueryAlgorithm v e r =
  Point 2 r -> VRS.VerticalRayShootingStructure v e r -> Maybe (LineSegment 2 v r :+ e)

queryWith         :: (Ord r, Fractional r)
                  => QueryAlgorithm v (Dart s) r
                  -> Point 2 r -> PointLocationDS s v e f r -> Maybe (Dart s)
queryWith query q = fmap (view extra) . query q . view verticalRayShootingStructure

-- | Locates the face containing the query point.
--
-- running time: \(O(\log n)\)
faceContaining      :: (Ord r, Fractional r)
                    => Point 2 r -> PointLocationDS s v e f r -> f
faceContaining q ds = ds^.subdivision.dataOf (faceIdContaining q ds)

-- | Locates the faceId of the face containing the query point.
--
-- running time: \(O(\log n)\)
faceIdContaining      :: (Ord r, Fractional r)
                      => Point 2 r -> PointLocationDS s v e f r -> FaceId' s
faceIdContaining q ds = maybe (ds^.outerFace) getFace $ dartAbove q ds
  where
    ps = ds^.subdivision
    getFace d = let (u,v) = bimap (^.location) (^.location) $ endPointData d ps
                in if u <= v then rightFace d ps
                             else leftFace  d ps
  -- TODO: describe what happens when you are on an edge

--------------------------------------------------------------------------------

-- | Data structure for fast InPolygon Queries
-- newtype InPolygonDS v r = InPolygonDS (VRS.VerticalRayShootingStructure (Vertex v r) () r)
--   deriving (Show,Eq)

data InOut = In | Out deriving (Show,Eq)

data Dummy
type InPolygonDS v r = PointLocationDS Dummy v () InOut  r


-- type Vertex v r = Int :+ (Point 2 r :+ v)

inPolygonDS    :: (Fractional r, Ord r) => SimplePolygon v r -> InPolygonDS v r
inPolygonDS pg = pointLocationDS $ fromSimplePolygon (Proxy @Dummy) pg In Out

edgeOnOrAbove      :: Point 2 r -> InPolygonDS v r -> Maybe (LineSegment 2 v r :+ (Int,Int))
edgeOnOrAbove q ds = undefined

pointInPolygon :: Point 2 r -> InPolygonDS v r -> InOut
pointInPolygon = undefined -- faceContaining
  -- FIXME: Make sure to also test the edge "below" q, i.e. if q is on
  -- some edge we should return that edge.

  -- FIXME: Figure out if this works ok for vertical edges as well
