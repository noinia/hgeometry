module Algorithms.Geometry.PointLocation.PersistentSweep where

import qualified Algorithms.Geometry.VerticalRayShooting.PersistentSweep as VRS
import           Control.Lens hiding (contains, below)
import           Data.Ext
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as V

--------------------------------------------------------------------------------

data PointLocationDS s v e f r = PointLocationDS {
        _verticalRayShootingStructure :: VRS.VerticalRayShootingStructure v (Dart s) r
      , _subdivision                  :: PlanarSubdivision s v e f r
      , _outerFace                    :: FaceId' s
      } deriving (Show,Eq)

verticalRayShootingStructure :: Getter (PointLocationDS s v e f r)
                                       (VRS.VerticalRayShootingStructure v (Dart s) r)
verticalRayShootingStructure = to _verticalRayShootingStructure

subdivision :: Getter (PointLocationDS s v e f r) (PlanarSubdivision s v e f r)
subdivision = to _subdivision

outerFace   :: Getter (PointLocationDS s v e f r) (FaceId' s)
outerFace   = to _outerFace


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
