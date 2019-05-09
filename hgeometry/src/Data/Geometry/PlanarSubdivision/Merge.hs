module Data.Geometry.PlanarSubdivision.Merge where

import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.PlanarSubdivision.Basic
import           Data.Geometry.PlanarSubdivision.Raw
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.PlanarGraph.Dart
import qualified Data.PlaneGraph as PG
import           Data.PlaneGraph( PlaneGraph, PlanarGraph, dual
                                , Dart, VertexId(..), FaceId(..), twin
                                , World(..)
                                , VertexId', FaceId'
                                , VertexData, location, vData
                                , HasDataOf(..)
                                )
import           Data.Proxy
import qualified Data.Sequence as Seq
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV


-- | Merge a pair of *disjoint* planar subdivisions!
merge = mergeWith const

-- | Merge a pair of *disjoint* planar subdivisions!
-- the functions specifies what to do with the outer face.
mergeWith         :: (f -> f -> f)
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

mergeFaceData f vs1 vs2 = let ts = V.tail vs1 <> V.tail vs2
                              h  = mergeOuterFaces f (V.head vs1) (V.head vs2)
                          in V.cons h ts

mergeOuterFaces f (FaceData hs1 x1)  (FaceData hs2 x2) = FaceData (hs1 <> hs2) (f x1 x2)


-- | applies a function to the first value of a vector
onHead     :: (a -> a) -> V.Vector a -> V.Vector a
onHead f v = v&ix 0 %~ f


    -- TODO: We still have to do something about the outer face I think.

-- | Shift the indices in a planar subdiv by the given numbers
-- (componentId;vertexId,darts,faceIds). Note that the result is not really a
-- valid planar subdivision, so be careful when using this!
shift                                             :: forall s v e f r.
                                                     Int -> Int -> Int -> Int
                                                  -> PlanarSubdivision s v e f r
                                                  -> PlanarSubdivision s v e f r
shift nc nv nd nf (PlanarSubdivision cs vd rd rf) = PlanarSubdivision cs' vd' rd' rf'
  where
    cs' = (\pg -> pg&PG.vertexData.traverse  %~ fv
                    &PG.rawDartData.traverse %~ fd
                    &PG.faceData.traverse    %~ ff
          ) <$> cs
    vd' = (\(Raw ci i x) -> Raw (fc ci) i x) <$> vd
    rd' = (\(Raw ci i x) -> Raw (fc ci) i x) <$> rd
    rf' = (\(Raw ci i x) -> Raw (fc ci) i x) <$> rf

    fc                 :: ComponentId s -> ComponentId s
    fc (ComponentId i) = ComponentId $ i + nc

    fv              :: VertexId' s -> VertexId' s
    fv (VertexId i) = VertexId $ i + nv

    fd                  :: Dart s -> Dart s
    fd (Dart (Arc a) p) = Dart (Arc $ a + nd) p

    ff (FaceData hs fi) = FaceData (fd <$> hs) (ffi fi)

    ffi                       :: FaceId' s -> FaceId' s
    ffi (FaceId (VertexId i)) = FaceId . VertexId $ i + nf
