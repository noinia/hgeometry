module Algorithms.Geometry.PolygonTriangulation.Types where

import           Control.Lens
import           Control.Monad (forM_)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision.Core
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.PlanarGraph as PG
import           Data.Semigroup
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
constructSubdivision                  :: (Num r, Ord r)
                                      => proxy s
                                      -> LineSegment 2 p r -- ^ A counter-clockwise
                                                         -- edge along the outer
                                                         -- boundary
                                      -> [LineSegment 2 p r] -- ^ remaining original edges
                                      -> [LineSegment 2 p r] -- ^ diagonals
                                      -> PlanarSubdivision s
                                            p PolygonEdgeType PolygonFaceData r
constructSubdivision px e origs diags =
    subdiv & graph.PG.vertexData.traverse.vData  %~ NonEmpty.head
           & graph.PG.faceData                   .~ faceData'
           & graph.PG.rawDartData.traverse.eData %~ snd
  where
    subdiv = fromConnectedSegments px $ e' : origs' <> diags'

    diags' = (:+ EdgeData Visible (True, Diagonal)) <$> diags
    origs' = (:+ EdgeData Visible (False,Original)) <$> origs
    e'     = e :+ EdgeData Visible (True, Original)

    g = subdiv^.graph

    -- the darts incident to internal faces
    queryDarts = concatMap shouldQuery . F.toList . PG.edges' $ g
    shouldQuery d = case g^.PG.eDataOf d.eData of
                      (True, Original) -> [d]
                      (True, Diagonal) -> [d, twin d]
                      _                -> []

    -- the interior faces
    intFaces = flip PG.leftFace g <$> queryDarts
    faceData' = V.create $ do
                  v' <- MV.replicate (PG.numFaces g) (FaceData [] Outside)
                  forM_ intFaces $ \(PG.FaceId (PG.VertexId f)) ->
                    MV.write v' f (FaceData [] Inside)
                  pure v'
