{-# LANGUAGE PartialTypeSignatures #-}
module Data.PlaneGraphSpec where


import           Control.Lens
import           Data.Bifunctor
import qualified Data.ByteString as B
import           Data.Ext
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.PlaneGraph
import           Data.PlaneGraph.IO(makeCCW)
import           Data.PlaneGraph.AdjRep
import           Data.Proxy
import           Data.Util
import qualified Data.Vector as V
import           Data.Yaml (prettyPrintParseException)
import           Data.Yaml.Util
import           Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "PlaneGraph tests" $ do
         it "fromConnectedSegments, correct handling of high degree vertex" $ do
           draw test `shouldBe` mempty
           draw test2 `shouldBe` mempty
         it "encode yaml test" $ do
           b <- B.readFile "test/Data/PlaneGraph/myPlaneGraph.yaml"
           encodeYaml myGraph `shouldBe` b
         it "from simple polygon; inside and outside correct" $
           outerFaceId simplePgGraph `shouldBe` (FaceId (VertexId 1))
         -- it "decode yaml test" $ do
         --   (first prettyPrintParseException
         --     <$> decodeYamlFile "test/Data/myPlaneGraph.yaml")
         --   `shouldReturn`
         --   (Right myGraph)
        -- the result is the same up to renumbering it seems. That is fine.
  where
    myGraph = fromConnectedSegments (Identity Test1) testSegs


data Test1 = Test1

draw  :: PlaneGraph s p e extra r -> V.Vector (FaceId' s, Polygon 'Simple p r :+ extra)
draw = V.filter isEmpty . rawFacePolygons
  where
    isEmpty (_,p :+ _) = (< 3) . length . polygonVertices $ p

test :: PlaneGraph Test1 _ () () Integer
test = fromConnectedSegments (Identity Test1) testSegs

test2 :: PlaneGraph Test1 _ () () Integer
test2 = fromConnectedSegments (Identity Test1) testSegs2

-- |
--
-- ![myGraph](test/Data/PlaneGraph/testsegs.png)
testSegs :: [LineSegment 2 () Integer :+ ()]
testSegs = map (\(p,q) -> ClosedLineSegment (ext p) (ext q) :+ ())
                   [ (origin, Point2 10 10)
                   , (origin, Point2 12 10)
                   , (origin, Point2 20 5)
                   , (origin, Point2 13 20)
                   , (Point2 10 10, Point2 12 10)
                   , (Point2 10 10, Point2 13 20)
                   , (Point2 12 10, Point2 20 5)
                   ]
testSegs2 :: [LineSegment 2 () Integer :+ ()]
testSegs2 = map (\(p,q) -> ClosedLineSegment (ext p) (ext q) :+ ())
                   [ (origin, Point2 10 0)
                   , (Point2 10 0, Point2 10 10)
                   , (origin, Point2 10 10)

                   , (origin, Point2 (-10) 0)
                   , (Point2 (-10) 0, Point2 (-10) (-10))
                   , (origin, Point2 (-10) (-10))
                   ]


--------------------------------------------------------------------------------

triangle :: Gr (Vtx Int String Int) (Face String)
triangle = Gr [ Vtx 0 (Point2 0 0) [ (2,"0->2")
                                , (1,"0->1")
                                ] 0
           , Vtx 1 (Point2 2 2) [ (0,"1->0")
                                , (2,"1->2")
                                ] 1
           , Vtx 2 (Point2 2 0) [ (0,"2->0")
                                , (1,"2->1")
                                ] 2
           ]
           [ Face (2,1) "OuterFace"
           , Face (0,1) "A"
           ]

smallG = fromAdjRep (Proxy :: Proxy ()) small

small :: Gr (Vtx Int String Int) (Face String)
small = Gr [ Vtx 0 (Point2 0 0) [ (2,"0->2")
                                , (1,"0->1")
                                , (3,"0->3")
                                ] 0
           , Vtx 1 (Point2 2 2) [ (0,"1->0")
                                , (2,"1->2")
                                , (3,"1->3")
                                ] 1
           , Vtx 2 (Point2 2 0) [ (0,"2->0")
                                , (1,"2->1")
                                ] 2
           , Vtx 3 (Point2 (-1) 4) [ (0,"3->0")
                                   , (1,"3->1")
                                   ] 3
           ]
           [ Face (2,1) "OuterFace"
           , Face (0,1) "A"
           , Face (1,0) "B"
           ]


data Test
myGraphG = fromAdjRep (Proxy :: Proxy Test) myGraph

myGraph :: Gr (Vtx () () Int) (Face String)
myGraph = makeCCW myGraph'

myGraph' :: Gr (Vtx () () Int) (Face String)
myGraph' = Gr [ Vtx 0 (Point2 0 0) [ (1,())
                                  , (5,())
                                  , (9,())
                                  , (2,())
                                  ] ()
             , Vtx 1 (Point2 4 4) [ (0,())
                                  , (5,())
                                  , (12,())
                                  ] ()
             , Vtx 2 (Point2 3 7) [ (3,())
                                  , (0,())
                                  ] ()
             , Vtx 3 (Point2 0 5) [(4,())
                                  , (2,())
                                  ] ()
             , Vtx 4 (Point2 3 8) [ (3,())
                                  , (13,())
                                  ] ()
             , Vtx 5 (Point2 8 1) [ (1,())
                                  , (0,())
                                  , (6,())
                                  , (8,())
                                  ] ()
             , Vtx 6 (Point2 6 (-1)) [ (5,())
                                     , (9,())
                                     ] ()
             , Vtx 7 (Point2 9 (-1)) [ (8,())
                                     , (11,())
                                     ] ()
             , Vtx 8 (Point2 12 1) [ (5,())
                                   , (7,())
                                   , (12,())
                                   ] ()
             , Vtx 9 (Point2 8 (-5)) [ (6,())
                                     , (0,())
                                     , (10,())
                                     ] ()
             , Vtx 10 (Point2 12 (-3)) [ (9,())
                                       , (11,())
                                       ] ()
             , Vtx 11 (Point2 14 (-1)) [ (10,())
                                       , (7,())
                                       ] ()
             , Vtx 12 (Point2 10 4) [ (8,())
                                    , (14,())
                                    , (1,())
                                    , (13,())
                                    ] ()
             , Vtx 13 (Point2 9 6) [ (4,())
                                   , (14,())
                                   , (12,())
                                   ] ()
             , Vtx 14 (Point2 8 5) [ (13,())
                                   , (12,())
                                   ] ()
             ]
             [ Face (4,3) "OuterFace"
             , Face (0,5) "A"
             , Face (1,5) "B"
             , Face (4,13) "C"
             , Face (13,12) "D"
             , Face (8,5) "E"
             , Face (9,6) "F"
             ]

data InOrOut = In | Out deriving (Show,Eq)

simplePgGraph :: Num r => PlaneGraph Test1 () () InOrOut r
simplePgGraph = fromSimplePolygon (Identity Test1) pg In Out
  where
    pg = fromPoints . map ext $ [ Point2 144 160
                                , Point2 64 96
                                , Point2 128 32
                                , Point2 208 96
                                ]
