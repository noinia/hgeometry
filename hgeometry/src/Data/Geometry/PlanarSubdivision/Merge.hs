--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.PlanarSubdivision.Merge
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Functions for merging two planar subdivisions
--
--------------------------------------------------------------------------------
module Data.Geometry.PlanarSubdivision.Merge( merge
                                            , mergeWith
                                            , mergeAllWith

                                            , embedAsHoleIn
                                            , embedAsHolesIn
                                            ) where

import           Algorithms.DivideAndConquer
import           Control.Lens hiding (holes)
import           Data.Ext
import           Data.Geometry.PlanarSubdivision.Basic
import           Data.Geometry.PlanarSubdivision.Raw
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.PlanarGraph.Dart
import           Data.PlaneGraph ( Dart, VertexId(..), FaceId(..)
                                , VertexId', FaceId'
                                )
import qualified Data.PlaneGraph as PG
import           Data.Semigroup.Foldable
import qualified Data.Vector as V
import           Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- * Embedding one subdivision in another one


embedAsHolesIn      :: forall t s h v e f r. (Foldable1 t, Functor t)
                    => t (PlanarSubdivision h v e f r) -- ^ The disjoint "holes"
                    -> (t f -> f -> f) -- ^ How to merge the face data
                    -> FaceId' s -- ^ Face in which to embed the given subdivisions
                    -> PlanarSubdivision s v e f r -- ^ the outer subdivision
                    -> PlanarSubdivision s v e f r
embedAsHolesIn hs f = embedAsHoleIn ph' g
  where
    -- merges all holes into one subdivision
    ph' = mergeAllWith const hs
    -- the new data value to use for the face i
    g _ = f (fmap (\h -> h^.dataOf (outerFaceId h)) hs)

embedAsHoleIn           :: forall s h v e f r.
                           PlanarSubdivision h v e f r -- ^ The hole
                        -> (f -> f -> f) -- ^ How to merge the face data (hole value first)
                        -> FaceId' s -- ^ Face in which to embed the given subdivisions
                        -> PlanarSubdivision s v e f r -- ^ the outer subdivision
                        -> PlanarSubdivision s v e f r
embedAsHoleIn ph' f i ps = mergeWith' mergeFaces ps ph
  where
    -- coerce the worlds to be the same
    ph :: PlanarSubdivision s v e f r
    ph = unsafeCoerce ph'
      -- We are coercing the 'h' into an 's' here. Since these
      -- parameters are phantom types the representation of the data
      -- is the same, and hence the unsafeCoerce should be safe here.

    mergeFaces fs1 fs2 = update fs1 i (V.head fs2) <> V.tail fs2

    update fs (FaceId (VertexId j)) h2 = let FaceData hs' x' = h2^.faceDataVal
                                             g (FaceData hs x) = FaceData (hs' <> hs) (f x' x)
                                         in fs&ix j.faceDataVal %~ g

  -- (PlanarSubdivision cs vd rd rf)&faceDataOf i %~ updateFData
  -- where
  --   -- shift p2
  --   p2' :: PlanarSubdivision s v e f r
  --   p2' = unsafeCoerce p2''

  --   p2'' :: PlanarSubdivision h v e f r
  --   p2'' = shift (numComponents ps) (numVertices ps) (numDarts ps `div` 2) (numFaces ps) ph
  --       -- we have to shift the number of the *Arcs*. Since every dart consists
  --       -- of two arcs, we have to shift by numDarts / 2

  --   -- merges all holes into one subdivision
  --   ph = mergeAllWith const hs

  --   cs = ps^.components <> p2'^.components
  --   vd = ps^.rawVertexData <> p2'^.rawVertexData
  --   rd = ps^.rawDartData <> p2'^.rawDartData
  --   rf = ps^.rawFaceData <> (V.tail $ p2'^.rawFaceData)

  --   -- the new data value to use for the face i
  --   x = f ofData (ps^.dataOf i)
  --   ofData = fmap (\h -> h^.dataOf (outerFaceId h)) hs

  --   updateFData (FaceData hs' _) = FaceData (newHs <> hs') x
  --   newHs = p2'^?!rawFaceData.ix 0.faceDataVal.holes





--------------------------------------------------------------------------------
-- * Merging Disjoint Subdivisions

-- | Merge a pair of *disjoint* planar subdivisions, unifying their
-- outer face. The given function is used to merge the data
-- corresponding to the outer face. The subdivisions are merged pairwise, no
-- guarantees are given about the order in which they are merged. Hence,
-- it is expected that f is commutative.
--
-- running time: \(O(n\log n)\), where \(n\) is the total size of the
-- subdivisions.
mergeAllWith   :: Foldable1 t
               => (f -> f -> f)
               -> t (PlanarSubdivision s v e f r)
               -> PlanarSubdivision s v e f r
mergeAllWith f = divideAndConquer1With (mergeWith f) id . toNonEmpty

-- | Merge a pair of *disjoint* planar subdivisions, unifying their
-- outer face. For the outerface data it simply takes the data of the
-- first subdivision.
--
-- runningtime: \(O(n)\)
merge :: PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
merge = mergeWith const

-- | Merge a pair of *disjoint* planar subdivisions. In particular,
-- this function unifies the structure assuming that the two
-- subdivisions share the outer face.
--
-- runningtime: \(O(n)\)
mergeWith   :: (f -> f -> f) -- ^  how to merge the data of the outer face
            -> PlanarSubdivision s v e f r
            -> PlanarSubdivision s v e f r
            -> PlanarSubdivision s v e f r
mergeWith f = mergeWith' (mergeFaceData f)

-- | Takes care of actually combining the vectors with data.
-- only thing left is how to merge the raw face data
mergeWith'  :: (V.Vector (RawFace s f) -> V.Vector (RawFace s f) -> V.Vector (RawFace s f))
             -- ^  how to merge the raw face data
            -> PlanarSubdivision s v e f r
            -> PlanarSubdivision s v e f r
            -> PlanarSubdivision s v e f r
mergeWith' mergeFaces p1 p2 = PlanarSubdivision cs vd rd rf
  where
    -- shift p2
    p2' = shift (numComponents p1) (numVertices p1) (numDarts p1 `div` 2) (numFaces p1 - 1) p2
        -- we have to shift the number of the *Arcs*. Since every dart
        -- consists of two arcs, we have to shift by numDarts / 2
        -- Furthermore, we take numFaces - 1 since we want the first
        -- /internal/ face of p2 (the one with FaceId 1) to correspond with the first free
        -- position (at index numFaces)

    cs = p1^.components <> p2'^.components
    vd = p1^.rawVertexData <> p2'^.rawVertexData
    rd = p1^.rawDartData <> p2'^.rawDartData
    rf = (p1^.rawFaceData) `mergeFaces` (p2'^.rawFaceData)

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

--------------------------------------------------------------------------------
-- * Implementation Helpers

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

    incC                 :: ComponentId s -> ComponentId s
    incC (ComponentId i) = ComponentId $ i + nc

    incV              :: VertexId' s -> VertexId' s
    incV (VertexId i) = VertexId $ i + nv

    incD                  :: Dart s -> Dart s
    incD (Dart (Arc a) p) = Dart (Arc $ a + nd) p

    incFIdx (ci,fi) = (incC ci, fi)
      -- observe that the fi here is the fi with respect to its original graph. Hence,
      -- we do not want to increase those id's

    incF                 :: FaceData (Dart s) f -> FaceData (Dart s) f
    incF (FaceData hs f) = FaceData (incD <$> hs) f

    incFi                       :: FaceId' s -> FaceId' s
    incFi (FaceId (VertexId i)) = FaceId . VertexId $ i + nf


--------------------------------------------------------------------------------

data Test = Test
data Id a = Id a


triangle1 :: PlanarSubdivision Test () () Int Rational
triangle1 = (\pg -> fromSimplePolygon (Id Test) pg 1 0)
          $ trianglePG1
trianglePG1 :: SimplePolygon () Rational
trianglePG1 = fromPoints . map ext $ [origin, Point2 200 0, Point2 200 200]


triangle2 :: PlanarSubdivision Test () () Int Rational
triangle2 = (\pg -> fromSimplePolygon (Id Test) pg 2 0)
          $ trianglePG2
trianglePG2 :: SimplePolygon () Rational
trianglePG2 = fromPoints . map ext $ [Point2 0 30, Point2 10 30, Point2 10 40]



triangle4 :: PlanarSubdivision Test () () Int Rational
triangle4 = (\pg -> fromSimplePolygon (Id Test) pg 1 0)
          $ trianglePG4
trianglePG4 :: SimplePolygon () Rational
trianglePG4 = fromPoints . map ext $ [Point2 400 400, Point2 600 400, Point2 600 600]

triangle3 :: PlanarSubdivision Test () () Int Rational
triangle3 = (\pg -> fromSimplePolygon (Id Test) pg 3 0)
          $ trianglePG3
trianglePG3 :: SimplePolygon () Rational
trianglePG3 = fromPoints . map ext $ [Point2 401 530, Point2 410 530, Point2 410 540]


_myPS :: PlanarSubdivision Test () () Int Rational
_myPS = embedAsHoleIn triangle2 const (mkFI 1) triangle1
       `merge`
       embedAsHoleIn triangle3 const (mkFI 1) triangle4


mkFI :: Int -> FaceId' Test
mkFI  = FaceId . VertexId
