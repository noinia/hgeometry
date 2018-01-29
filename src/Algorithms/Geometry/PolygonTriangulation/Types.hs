module Algorithms.Geometry.PolygonTriangulation.Types where

import           Control.Lens
import           Control.Monad (forM_)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision.Basic
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.PlaneGraph as PG
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
constructSubdivision px e origs diags = undefined


  --   subdiv & planeGraph.PG.vertexData.traverse  %~ NonEmpty.head
  --          & faceData             .~ faceData'
  --          & planeGraph.PG.rawDartData.traverse %~ snd
  -- where
  --   subdiv = fromConnectedSegments px $ e' : origs' <> diags'

  --   diags' = (:+ (True, Diagonal)) <$> diags
  --   origs' = (:+ (False,Original)) <$> origs
  --   e'     = e :+ (True, Original)

  --   -- g = subdiv^.planeGraph

  --   -- the darts incident to internal faces
  --   queryDarts = concatMap shouldQuery . F.toList . PG.edges' $ g
  --   shouldQuery d = case subdiv^.dataOf d of
  --                     (True, Original) -> [d]
  --                     (True, Diagonal) -> [d, twin d]
  --                     _                -> []

  --   -- the interior faces
  --   intFaces = flip leftFace subdiv <$> queryDarts
  --   faceData' = V.create $ do
  --                 v' <- MV.replicate (numFaces subdiv) (FaceData mempty Outside)
  --                 forM_ intFaces $ \(PG.FaceId (PG.VertexId f)) ->
  --                   MV.write v' f (FaceData mempty Inside)
  --                 pure v'
