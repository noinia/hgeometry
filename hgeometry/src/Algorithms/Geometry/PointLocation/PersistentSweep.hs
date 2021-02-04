{-# Language TemplateHaskell #-}
module Algorithms.Geometry.PointLocation.PersistentSweep
  ( PointLocationDS(PointLocationDS)
  , verticalRayShootingStructure, subdivision, outerFace

  -- * Building the Data Structure
  , pointLocationDS
  -- * Querying the Data Structure
  , dartAbove
  , faceContaining
  ) where

import qualified Algorithms.Geometry.VerticalRayShooting.PersistentSweep as VRS
import           Control.Lens hiding (contains, below)
import           Data.Ext
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import qualified Data.List.NonEmpty as NonEmpty
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

-- TODO: Figure out what to do with vertical edges


--------------------------------------------------------------------------------
-- * Querying the Structure

-- | Locates the edge (dart) directly above the query point.
-- returns Nothing if the query point lies in the outer face and there is no dart
-- above it.
--
-- FIXME: Specify what happens when the query point lies on an edge
--
-- running time: \(O(\log n)\)
dartAbove   :: (Ord r, Fractional r)
            => Point 2 r -> PointLocationDS s v e f r -> Maybe (Dart s)
dartAbove q = fmap (view extra) . VRS.segmentAbove q . view verticalRayShootingStructure

-- | Locates the face containing the query point.
--
-- running time: \(O(\log n)\)
faceContaining      :: (Ord r, Fractional r)
                    => Point 2 r -> PointLocationDS s v e f r -> FaceId' s
faceContaining q ds = maybe (ds^.outerFace) getFace $ dartAbove q ds
  where
    ps = ds^.subdivision
    getFace d = let (u,v) = bimap (^.location) (^.location) $ endPointData d ps
                in if u <= v then rightFace d ps
                             else leftFace  d ps

--------------------------------------------------------------------------------

-- | Data structure for fast InPolygon Queries
newtype InPolygonDS v r = InPolygonDS (VRS.VerticalRayShootingStructure (Vertex v r) () r)
  deriving (Show,Eq)

type Vertex v r = Int :+ (Point 2 r :+ v)

inPolygonDS    :: SimplePolygon v r -> InPolygonDS v r
inPolygonDS pg = undefined

edgeOnOrAbove      :: Point 2 r -> InPolygonDS v r -> Maybe (LineSegment 2 v r :+ (Int,Int))
edgeOnOrAbove q ds = undefined

pointInPolygon :: Point 2 r -> InPolygonDS v r -> PointLocationResult
pointInPolygon q ds = undefined
  -- FIXME: Make sure to also test the edge "below" q, i.e. if q is on
  -- some edge we should return that edge.

  -- FIXME: Vertical sides are important here. They are ignored by the structure, so if the query
  -- point lies on such an edge we don't know it
