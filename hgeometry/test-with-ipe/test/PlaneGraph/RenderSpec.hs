{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module PlaneGraph.RenderSpec
  ( spec
  , drawGraph
  , drawVertex
  , drawDart
  , drawFace
  , drawEdge
  ) where

import           Control.Lens
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import           Golden
import           HGeometry.Ext
import           HGeometry.Line
import           HGeometry.LineSegment
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Polygon.Simple
import           HGeometry.Transformation
import           HGeometry.Vector
-- import           HGeometry.YAML
import qualified Hiraffe.PlanarGraph.AdjRep as AdjRep
import           Hiraffe.PlanarGraph.IO ()
import qualified Hiraffe.PlanarGraph.IO as AdjRep
import           Ipe
import qualified Ipe.Color as Ipe
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck.Instances ()
--------------------------------------------------------------------------------




-- >>> import qualified Data.Vector.NonEmpty as V
-- >>> import qualified Data.List.NonEmpty as NonEmpty
-- >>> import Control.Lens
-- >>> import Hiraffe.PlanarGraph.Dart(Dart(Dart),Arc(Arc),Direction(..))
-- >>> :{
-- let dart i s = Dart (Arc i) (read s)
--     (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]
--     adjacencies = NonEmpty.fromList . fmap NonEmpty.fromList $
--                           [ [ (Dart aA Negative, "a-")
--                             , (Dart aC Positive, "c+")
--                             , (Dart aB Positive, "b+")
--                             , (Dart aA Positive, "a+")
--                             ]
--                           , [ (Dart aE Negative, "e-")
--                             , (Dart aB Negative, "b-")
--                             , (Dart aD Negative, "d-")
--                             , (Dart aG Positive, "g+")
--                             ]
--                           , [ (Dart aE Positive, "e+")
--                             , (Dart aD Positive, "d+")
--                             , (Dart aC Negative, "c-")
--                             ]
--                           , [ (Dart aG Negative, "g-")
--                             ]
--                           ]
--     myGraph :: PlanarGraph String Primal String String String
--     myGraph = planarGraph  adjacencies
--                  & vertexData .~ V.unsafeFromList ["u","v","w","x"]
--                  & faceData   .~ V.unsafeFromList ["f_3", "f_infty","f_1","f_2"]
-- :}
--




-- data MyWorld

type R = Double

spec :: Spec
spec = describe "render planegraph tests" $ do
         goldenWith [osp|data/test-with-ipe/golden/PlaneGraph|]
                    (ipeContentGolden { name = [osp|smallPlaneGraph|]})
                    (drawGraph smallGraph)
         -- eg <- runIO $ decodeYAMLFile [osp|myPlaneGraph.json|]
         -- let myPlaneGraph = case eg of
         --       Left err -> error (show err)
         --       Right (g :: PlaneGraph MyWorld (Point 2 R) Text.Text Text.Text) -> g
         -- goldenWith [osp|data/golden|]
         --            (ipeContentGolden { name = [osp|myPlaneGraphRendered|]})
         --            (drawGraph myPlaneGraph)

--------------------------------------------------------------------------------

drawGraph    :: ( PlaneGraph_ planeGraph vertex, HasOuterBoundaryOf planeGraph
                , IsTransformable vertex
                , ConstructablePoint_ vertex 2 r, Ord r, Real r
                , Fractional r, Show r, Eq (FaceIx planeGraph)
                , Show (Vertex planeGraph), Show (Dart planeGraph), Show (Face planeGraph)
                , Show (EdgeIx planeGraph)
                ) => planeGraph -> [IpeObject r]
drawGraph gr = theVertices <> theEdges <> theFaces
  where
    theVertices = ifoldMapOf vertices             drawVertex    gr
    theEdges    = ifoldMapOf dartSegments         (drawDart gr) gr
               <> ifoldMapOf edgeSegments         (drawEdge gr) gr
    theFaces    = ifoldMapOf interiorFacePolygons (drawFace gr) gr

drawVertex     :: ( Point_ vertex 2 r, Show vertex)
               => i -> vertex -> [IpeObject r]
drawVertex _ v = [ iO $ ipeDiskMark (v^.asPoint) ! attr SLayer "vertex"
                 , iO $ ipeLabel (tshow v :+ v^.asPoint) ! attr SLayer "vertexLabel"
                                                         -- ! attr SStroke Ipe.red
                 ]

drawEdge        :: ( PlaneGraph_ planeGraph vertex, ConstructablePoint_ vertex 2 r, IsTransformable vertex
                   , Show (EdgeIx planeGraph), Fractional r, Real r)
                => planeGraph -> EdgeIx planeGraph -> ClosedLineSegment vertex -> [IpeObject r]
drawEdge _g d s = [ iO $ ipeLineSegment s ! attr SLayer "edges"
                  , iO $ ipeLabel (tshow d :+ c) ! attr SLayer "edgeLabel"
                  ]
  where
    c = interpolate 0.5 s ^. asPoint


drawDart        :: ( PlaneGraph_ planeGraph vertex, ConstructablePoint_ vertex 2 r, IsTransformable vertex
                   , Show (Dart planeGraph), Fractional r, Real r)
                => planeGraph -> DartIx planeGraph -> ClosedLineSegment vertex -> [IpeObject r]
drawDart gr d s = [ iO $ ipeLineSegment (offset s)
                         ! attr SArrow normalArrow
                         ! attr SStroke Ipe.purple
                         ! attr SLayer "darts"
                  , iO $ ipeLabel (tshow (gr^?!dartAt d) :+ c) ! attr SLayer "dartLabel"
                  ]
  where
    c = interpolate 0.5 s ^. asPoint
    -- computes the midpoint of the segment.

-- | slightly shift the segment
offset   :: forall lineSegment point r.
            (LineSegment_ lineSegment point, IsTransformable lineSegment
            , HasSupportingLine lineSegment
            , Point_ point 2 r, Real r, Fractional r)
         => lineSegment -> lineSegment
offset s = translateBy theOffset s
  where
    theOffset :: Vector 2 r
    theOffset = fmap realToFrac . negated $ signorm (realToFrac @_ @Double <$> v)
    v :: Vector 2 r
    v = perpendicularTo (supportingLine s) ^. direction

drawFace         :: ( PlaneGraph_ planeGraph vertex, Point_ vertex 2 r
                    , Show (Face planeGraph), Ord r, Fractional r)
                 => planeGraph -> FaceIx planeGraph -> SimplePolygon (vertex :+ VertexIx planeGraph) -> [IpeObject r]
drawFace gr f pg = [ iO $ ipeSimplePolygon pg' ! attr SLayer "face"
                   , iO $ ipeLabel (tshow (gr^?!faceAt f) :+ c) ! attr SLayer "faceLabel"
                   ]
  where
    pg' :: SimplePolygon _
    pg' = pg&vertices %~ (^.core.asPoint)
    c = centroid pg'


tshow :: Show a => a -> Text.Text
tshow = Text.pack . show


--------------------------------------------------------------------------------

data SmallWorld

smallGraph :: CPlaneGraph SmallWorld (Point 2 R :+ Int) Text.Text Text.Text
smallGraph = review _CPlanarGraph $ AdjRep.fromAdjRep @SmallWorld small
  where
    small :: AdjRep.Gr (AdjRep.Vtx (Point 2 R :+ Int) Text.Text) (AdjRep.Face Text.Text)
    small = AdjRep.Gr
               ( NonEmpty.fromList
                 [ AdjRep.Vtx 0 [ (2,"0->2")
                                , (1,"0->1")
                                , (3,"0->3")
                                ] (Point2 0 0 :+ 0)
                 , AdjRep.Vtx 1 [ (0,"1->0")
                                , (2,"1->2")
                                , (3,"1->3")
                              ] (Point2 200 200 :+ 1)
                 , AdjRep.Vtx 2 [ (0,"2->0")
                                , (1,"2->1")
                                ] (Point2 200 0 :+ 2)
                 , AdjRep.Vtx 3 [ (0,"3->0")
                                , (1,"3->1")
                                ] (Point2 (-100) 400 :+ 3)
                 ])
               (NonEmpty.fromList
                [ AdjRep.Face (2,1) "OuterFace"
                , AdjRep.Face (0,1) "A"
                , AdjRep.Face (1,0) "B"
                ])
