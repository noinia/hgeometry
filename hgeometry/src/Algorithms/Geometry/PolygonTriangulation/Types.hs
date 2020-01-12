{-# LANGUAGE ScopedTypeVariables #-}
module Algorithms.Geometry.PolygonTriangulation.Types where

import           Control.Lens
import           Control.Monad (forM_)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision.Basic
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.PlaneGraph as PG
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

--------------------------------------------------------------------------------

data PolygonEdgeType = Original | Diagonal
                     deriving (Show,Read,Eq)

-- | Given a list of original edges and a list of diagonals, creates a
-- planar-subdivision
--
--
-- running time: \(O(n\log n)\)
constructSubdivision                  :: forall proxy r s p. (Fractional r, Ord r)
                                      => proxy s
                                      -> LineSegment 2 p r -- ^ A counter-clockwise
                                                         -- edge along the outer
                                                         -- boundary
                                      -> [LineSegment 2 p r] -- ^ remaining original edges
                                      -> [LineSegment 2 p r] -- ^ diagonals
                                      -> PlanarSubdivision s
                                            p PolygonEdgeType PolygonFaceData r
constructSubdivision px e origs diags = fromPlaneGraph $ constructGraph px e origs diags

-- constructSubdivision px e origs diags =
--     subdiv & rawVertexData.traverse.dataVal  %~ NonEmpty.head
--            & rawFaceData                     %~ V.zipWith zipF faceData'
--            & rawDartData.traverse.dataVal    %~ snd
--   where
--     subdiv :: PlanarSubdivision s (NonEmpty p) (Bool,PolygonEdgeType) () r
--     subdiv = fromConnectedSegments px $ e' : origs' <> diags'

--     diags' = (:+ (True, Diagonal)) <$> diags
--     origs' = (:+ (False,Original)) <$> origs
--     e'     = e :+ (True, Original)

--     -- the darts incident to internal faces
--     queryDarts = concatMap shouldQuery . F.toList . edges' $ subdiv
--     shouldQuery d = case subdiv^.dataOf d of
--                       (True, Original) -> [d]
--                       (True, Diagonal) -> [d, twin d]
--                       _                -> []

--     -- the interior faces
--     intFaces = flip leftFace subdiv <$> queryDarts
--     faceData' = V.create $ do
--                   v' <- MV.replicate (numFaces subdiv) Outside
--                   forM_ intFaces $ \(PG.FaceId (PG.VertexId f)) ->
--                     MV.write v' f Inside
--                   pure v'

--     -- set the inside/outside data value
--     zipF x rfd = rfd&dataVal .~ x
-- -- TODO: Idea: generalize the face data assignment into a function
-- -- that does something like: [(Dart, fLeft, fRight] -> FaceData


-- | Given a list of original edges and a list of diagonals, creates a
-- planar-subdivision
--
--
-- running time: \(O(n\log n)\)
constructGraph                  :: forall proxy r s p. (Fractional r, Ord r)
                                      => proxy s
                                      -> LineSegment 2 p r -- ^ A counter-clockwise
                                                         -- edge along the outer
                                                         -- boundary
                                      -> [LineSegment 2 p r] -- ^ remaining original edges
                                      -> [LineSegment 2 p r] -- ^ diagonals
                                      -> PG.PlaneGraph s
                                            p PolygonEdgeType PolygonFaceData r
constructGraph px e origs diags =
    subdiv & PG.vertexData.traverse  %~ NonEmpty.head
           & PG.faceData             .~ faceData'
           & PG.rawDartData.traverse %~ snd
  where
    subdiv :: PG.PlaneGraph s (NonEmpty p) (Bool,PolygonEdgeType) () r
    subdiv = PG.fromConnectedSegments px $ e' : origs' <> diags'

    diags' = (:+ (True, Diagonal)) <$> diags
    origs' = (:+ (False,Original)) <$> origs
    e'     = e :+ (True, Original)

    -- the darts incident to internal faces
    queryDarts = concatMap shouldQuery . F.toList . PG.edges' $ subdiv
    shouldQuery d = case subdiv^.dataOf d of
                      (True, Original) -> [d]
                      (True, Diagonal) -> [d, twin d]
                      _                -> []

    -- the interior faces
    intFaces = flip PG.leftFace subdiv <$> queryDarts

    faceData' :: V.Vector PolygonFaceData
    faceData' = V.create $ do
                  v' <- MV.replicate (PG.numFaces subdiv) Outside
                  forM_ intFaces $ \(PG.FaceId (PG.VertexId f)) ->
                    MV.write v' f Inside
                  pure v'

-- -constructSubdivision px e origs diags =
-- -    subdiv & planeGraph.PG.vertexData.traverse        %~ NonEmpty.head
-- -           & planeGraph.PG.faceData                   .~ faceData'
-- -           & planeGraph.PG.rawDartData.traverse.eData %~ snd
-- -  where
-- -    subdiv = fromConnectedSegments px $ e' : origs' <> diags'
-- -
-- -    diags' = (:+ EdgeData Visible (True, Diagonal)) <$> diags
-- -    origs' = (:+ EdgeData Visible (False,Original)) <$> origs
-- -    e'     = e :+ EdgeData Visible (True, Original)
-- -
-- -    g = subdiv^.planeGraph
-- -
-- -    -- the darts incident to internal faces
-- -    queryDarts = concatMap shouldQuery . F.toList . PG.edges' $ g
-- -    shouldQuery d = case g^.dataOf d.eData of
-- -                      (True, Original) -> [d]
-- -                      (True, Diagonal) -> [d, twin d]
-- -                      _                -> []
-- -
-- -    -- the interior faces
-- -    intFaces = flip PG.leftFace g <$> queryDarts
-- -    faceData' = V.create $ do
-- -                  v' <- MV.replicate (PG.numFaces g) (FaceData [] Outside)
-- -                  forM_ intFaces $ \(PG.FaceId (PG.VertexId f)) ->
-- -                    MV.write v' f (FaceData [] Inside)
-- -                  pure v'
