{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.PointLocation.PersistentSweep
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Data.Geometry.PointLocation.PersistentSweep
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

import qualified Data.Geometry.VerticalRayShooting.PersistentSweep as VRS
import           Control.Lens hiding (contains, below)
import           Data.Ext
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Proxy
import           Data.Util (SP(..))
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
-- If the query point lies *on* an edge, an arbitrary face incident to
-- the edge is returned.
--
-- running time: \(O(\log n)\)
faceIdContaining      :: (Ord r, Fractional r)
                      => Point 2 r -> PointLocationDS s v e f r -> FaceId' s
faceIdContaining q ds = dartToFace ds $ dartAbove q ds

-- | Given the dart determine the faceId correspondig to it (depending
-- on the orientation of the dart that is returned.)
dartToFace    :: Ord r => PointLocationDS s v e f r -> Maybe (Dart s) -> FaceId' s
dartToFace ds = maybe (ds^.outerFace) getFace
  where
    ps = ds^.subdivision
    getFace d = let (u,v) = bimap (^.location) (^.location) $ endPointData d ps
                in if u <= v then rightFace d ps
                             else leftFace  d ps


data OneOrTwo a = One !a | Two !a !a deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

-- | Locates the faceId of the face containing the query point. If the
-- query point lies on an edge, it returns both faces incident to the
-- edge; first the one below the edge then the one above the edge.
--
-- running time: \(O(\log n)\)
faceIdContaining'      :: (Ord r, Fractional r)
                      => Point 2 r -> PointLocationDS s v e f r -> OneOrTwo (FaceId' s)
faceIdContaining' q ds = maybe (One $ ds^.outerFace) getFace $ dartAboveOrOn q ds
  where
    ps = ds^.subdivision

    getFace = getFace' . orient

    orient d = let (u,v) = bimap (^.location) (^.location) $ endPointData d ps
               in if u <= v then (d,u,v) else (twin d, v, u)


    getFace' (d,u,v) = case ccw u q v of
                         CoLinear -> Two (rightFace d ps) (leftFace d ps)
                         _        -> One (rightFace d ps)

--------------------------------------------------------------------------------

-- | Data structure for fast InPolygon Queries
-- newtype InPolygonDS v r = InPolygonDS (VRS.VerticalRayShootingStructure (Vertex v r) () r)
--   deriving (Show,Eq)

data InOut = In | Out deriving (Show,Eq)

data Dummy
type InPolygonDS v r = PointLocationDS Dummy (SP Int v) () InOut  r


-- type Vertex v r = Int :+ (Point 2 r :+ v)

inPolygonDS    :: (Fractional r, Ord r) => SimplePolygon v r -> InPolygonDS v r
inPolygonDS pg = pointLocationDS $ fromSimplePolygon (Proxy @Dummy) (numberVertices pg) In Out

-- | Finds the edge on or above the query point, if it exists
--
--
edgeOnOrAbove      :: (Ord r, Fractional r)
                   => Point 2 r -> InPolygonDS v r -> Maybe (LineSegment 2 (SP Int v) r)
edgeOnOrAbove q ds = view core . flip edgeSegment (ds^.subdivision) <$> dartAboveOrOn q ds


-- | Returns if a query point lies in (or on the boundary of) the polygon.
--
-- \(O(\log n)\)
pointInPolygon :: (Ord r, Fractional r) => Point 2 r -> InPolygonDS v r -> InOut
pointInPolygon q ds = case faceIdContaining' q ds of
                        One i   -> ds^.subdivision.dataOf i
                        Two _ _ -> In -- on an edge, so inside.

  -- FIXME: Make sure to also test the edge "below" q, i.e. if q is on
  -- some edge we should return that edge.

  -- FIXME: Figure out if this works ok for vertical edges as well
