--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Triangulation.Types
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Polygon.Triangulation.Types where

import           Control.Lens
import           Data.Coerce
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           HGeometry.Lens.Util
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Vector
import           Hiraffe.PlanarGraph as PlanarGraph

-- import qualified Data.Foldable as F
-- import Debug.Trace
--------------------------------------------------------------------------------

-- | After triangulation, edges are either from the original polygon or a new diagonal.
data PolygonEdgeType = Original | Diagonal
                     deriving (Show,Read,Eq)

-- | Data type that expresses whether or not we are inside or outside the
-- polygon.
data PolygonFaceData = Inside | Outside deriving (Show,Read,Eq)

type Diagonal polygon = Vector 2 (VertexIx polygon)


{-

-- | Given a list of original edges and a list of diagonals, creates a
-- planar-subdivision
--
--
-- running time: \(O(n\log n)\)
constructSubdivision               :: forall s r p. (Fractional r, Ord r)
                                   => LineSegment 2 p r -- ^ A counter-clockwise
                                                      -- edge along the outer
                                                      -- boundary
                                   -> [LineSegment 2 p r] -- ^ remaining original edges
                                   -> [LineSegment 2 p r] -- ^ diagonals
                                   -> PlanarSubdivision s
                                         p PolygonEdgeType PolygonFaceData r
constructSubdivision e origs diags = fromPlaneGraph $ constructGraph e origs diags

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

-}


-- fromConnectedSegments       :: ( Foldable1 f
--                                , LineSegment_ lineSegment vertex
--                                , Point_ vertex 2 r, Ord r, Num r
--                                )
--                             => f lineSegment -> PlaneGraph s (NonEmpty vertex) lineSegment ()
-- fromConnectedSegments segs = fromAdjacencyLists adjLists
--   where
--     adjLists = Map.fromListWith (<>) . concatMap f . zipWith g [0..] . F.toList $ ss

--     mkVertex



--   fromadjacencylists
--   where
-- planarGraph dts & PG.vertexData .~ vxData
--   where
--     pts         = M.fromListWith (<>) . concatMap f . zipWith g [0..] . F.toList $ ss
--     f (s :+ e)  = [ ( s^.start.core
--                     , SP (sing $ s^.start.extra) [(s^.end.core)   :+ h Positive e])
--                   , ( s^.end.core
--                     , SP (sing $ s^.end.extra)   [(s^.start.core) :+ h Negative e])
--                   ]
--     g i (s :+ e) = s :+ (Arc i :+ e)
--     h d (a :+ e) = (Dart a d, e)

--     sing x = x NonEmpty.:| []

--     vts    = map (\(p,sp) -> (p,map (^.extra) . sortAround' (ext p) <$> sp))
--            . M.assocs $ pts
--     -- vertex Data
--     vxData = V.fromList . map (\(p,sp) -> VertexData p (sp^._1)) $ vts
--     -- The darts
--     dts    = map (^._2._2) vts



-- | Given a list of original edges and a list of diagonals, creates a
-- planar-subdivision. The vertexId's will remain unchanged.
--
--
-- running time: \(O(n\log n)\)
constructGraph          :: forall s polygon point r f.
                           ( SimplePolygon_ polygon point r, Point_ point 2 r
                           , Foldable f, Ord r, Num r
                           )
                        => polygon
                        -> f (Diagonal polygon)
                        -> CPlaneGraph s point PolygonEdgeType PolygonFaceData
constructGraph pg diags = gr&faces %@~ computeFaceLabel
-- constructGraph pg diags = gr&faces %@~ computeFaceLabel
  where
    -- | Note that we use fromAdjacencyLists
    gr = fromAdjacencyLists adjLists :: CPlaneGraph s point PolygonEdgeType ()

    adjLists = uncurry collectDiags <$> itoNonEmptyOf outerBoundaryWithNeighbours pg

    collectDiags                  :: (VertexIx polygon, (VertexIx polygon, VertexIx polygon))
                                  -> (point,            (point,            point))
                                  -> ( VertexIdIn Primal s
                                     , point, NonEmpty (VertexIdIn Primal s,PolygonEdgeType)
                                     )
    collectDiags (u, (v,w)) (p,_) = coerce (u,p, (v, Original) :| (w, Original) : diagonalsOf u)

    -- get the diagonals incident to vertex u
    diagonalsOf u = fromMaybe [] $ Map.lookup u diags'
    -- associate every diagonal with its endpoints
    diags' :: Map.Map (VertexIx polygon) [(VertexIx polygon, PolygonEdgeType)]
    diags' = foldr (\(Vector2 u v) -> Map.insertWith (<>) u [(v, Diagonal)]
                                    . Map.insertWith (<>) v [(u, Diagonal)]
                   ) Map.empty diags

    theOuterFaceId = outerFaceId gr
    computeFaceLabel fi _
      | fi == theOuterFaceId = Outside
      | otherwise            = Inside



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


--------------------------------------------------------------------------------
