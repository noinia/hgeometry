--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.PlnarSubdivision.Merge
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Functions for merging two planar subdivisions
--
--------------------------------------------------------------------------------
module Data.Geometry.PlanarSubdivision.Merge( merge
                                            , mergeWith
                                            ) where

import           Control.Lens
import           Data.Geometry.PlanarSubdivision.Basic
import           Data.Geometry.PlanarSubdivision.Raw
import           Data.PlanarGraph.Dart
import qualified Data.PlaneGraph as PG
import           Data.PlaneGraph( Dart, VertexId(..), FaceId(..)
                                , VertexId', FaceId'
                                )
import qualified Data.Vector as V

--------------------------------------------------------------------------------

-- | Merge a pair of *disjoint* planar subdivisions, unifying their
-- outer face. For the outerface data it simply takes the data of the
-- first subdivision.
--
-- runningtime: \(O(n)\)
merge :: PlanarSubdivision s v e f r
      -> PlanarSubdivision s v e f r
      -> PlanarSubdivision s v e f r
merge = mergeWith const

-- | Merge a pair of *disjoint* planar subdivisions. In particular,
-- this function unifies the structure assuming that the two
-- subdivisions share the outer face.
--
-- runningtime: \(O(n)\)
mergeWith         :: (f -> f -> f) -- ^  how to merge the data of the outer face
                  -> PlanarSubdivision s v e f r
                  -> PlanarSubdivision s v e f r
                  -> PlanarSubdivision s v e f r
mergeWith f p1 p2 = PlanarSubdivision cs vd rd rf
  where
    -- shift p2
    p2' = shift (numComponents p1) (numVertices p1) (numDarts p1) (numFaces p1) p2

    cs = p1^.components <> p2'^.components
    vd = p1^.rawVertexData <> p2'^.rawVertexData
    rd = p1^.rawDartData <> p2'^.rawDartData
    rf = mergeFaceData f (p1^.rawFaceData) (p2'^.rawFaceData)

mergeFaceData           :: (f -> f -> f)
                        -> V.Vector (RawFace s f)
                        -> V.Vector (RawFace s f)
                        -> V.Vector (RawFace s f)
mergeFaceData f vs1 vs2 = V.cons h ts

  where
    ts = V.tail vs1 <> V.tail vs2
    h  = let FaceData hs1 x1 = vs1^.to V.head.faceDataVal
             FaceData hs2 x2 = vs2^.to V.head.faceDataVal
         in RawFace Nothing $ FaceData (hs1 <> hs2) (f x1 x2)

-- -- | applies a function to the first value of a vector
-- onHead     :: (a -> a) -> V.Vector a -> V.Vector a
-- onHead f v = v&ix 0 %~ f

-- | Shift the indices in a planar subdiv by the given numbers
-- (componentId;vertexId,darts,faceIds). Note that the result is not really a
-- valid planar subdivision, so be careful when using this!
shift                                             :: forall s v e f r.
                                                     Int -> Int -> Int -> Int
                                                  -> PlanarSubdivision s v e f r
                                                  -> PlanarSubdivision s v e f r
shift nc nv nd nf (PlanarSubdivision cs vd rd rf) = PlanarSubdivision cs' vd' rd' rf'
  where
    cs' = (\pg -> pg&PG.vertexData.traverse  %~ incV
                    &PG.rawDartData.traverse %~ incD
                    &PG.faceData.traverse    %~ incFi
          ) <$> cs
    vd' = (\(Raw ci i x)      -> Raw (incC ci) i x)                    <$> vd
    rd' = (\(Raw ci i x)      -> Raw (incC ci) i x)                    <$> rd
    rf' = (\(RawFace fidx fd) -> RawFace (incFIdx <$> fidx) (incF fd)) <$> rf

    incC                 :: forall s'. ComponentId s' -> ComponentId s'
    incC (ComponentId i) = ComponentId $ i + nc

    incV              :: VertexId' s -> VertexId' s
    incV (VertexId i) = VertexId $ i + nv

    incD                  :: Dart s -> Dart s
    incD (Dart (Arc a) p) = Dart (Arc $ a + nd) p

    incFIdx (ci,fi) = (incC ci, incFi fi)

    incF (FaceData hs f) = FaceData (incD <$> hs) f

    incFi                       :: forall s'. FaceId' s' -> FaceId' s'
    incFi (FaceId (VertexId i)) = FaceId . VertexId $ i + nf
