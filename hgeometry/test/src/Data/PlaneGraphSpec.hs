{-# LANGUAGE PartialTypeSignatures #-}
module Data.PlaneGraphSpec (spec) where

import           Control.Lens
import qualified Data.ByteString as B
import           Data.Ext
import           Data.Function (on)
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Ord (comparing)
import           Data.PlaneGraph
import           Data.PlaneGraph.AdjRep
import           Data.RealNumber.Rational
import qualified Data.Vector as V
import           Data.Yaml.Util
import           Test.Hspec

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "PlaneGraph tests" $ do
         it "fromConnectedSegments, correct handling of high degree vertex" $ do
           draw test `shouldBe` mempty
           draw test2 `shouldBe` mempty
         it "outerBoundaryVertices" $ do
           (fmap (\i -> myGraph^.locationOf i) $ boundaryVertices (outerFaceId myGraph) myGraph)
             `shouldBe` outerBoundaryVerticesTestSegs
         it "encode yaml test" $ do
           b <- B.readFile "test/src/Data/PlaneGraph/myPlaneGraph.yaml"
           encodeYaml myGraph `shouldBe` b
         it "from simple polygon; inside and outside correct" $
           outerFaceId simplePgGraph `shouldBe` FaceId (VertexId 1)
         it "right orientations" $
           allFaceOrientations myGraph `shouldBe` True
         it "outerfaceDart still the same" $
           outerFaceDart myGraph `shouldBe` outerFaceDartFractional myGraph
         triangleSpec

         -- it "decode yaml test" $ do
         --   (first prettyPrintParseException
         --     <$> decodeYamlFile "src/Data/myPlaneGraph.yaml")
         --   `shouldReturn`
         --   (Right myGraph)
        -- the result is the same up to renumbering it seems. That is fine.
  where
    myGraph = fromConnectedSegments @Test1 testSegs


data Test1

draw    :: (Ord r, Fractional r)
        => PlaneGraph s p e extra r -> V.Vector (FaceId' s, Polygon 'Simple p r :+ extra)
draw g = V.filter isEmpty . snd $ facePolygons (outerFaceId g) g
  where
    isEmpty (_,p :+ _) = (< 3) . length . polygonVertices $ p

test :: PlaneGraph Test1 _ () () R
test = fromConnectedSegments @Test1 testSegs

test2 :: PlaneGraph Test1 _ () () R
test2 = fromConnectedSegments @Test1 testSegs2

-- |
--
-- ![myGraph](src/Data/PlaneGraph/testsegs.png)
testSegs :: [LineSegment 2 () R :+ ()]
testSegs = map (\(p,q) -> ClosedLineSegment (ext p) (ext q) :+ ())
                   [ (origin, Point2 10 10)
                   , (origin, Point2 12 10)
                   , (origin, Point2 20 5)
                   , (origin, Point2 13 20)
                   , (Point2 10 10, Point2 12 10)
                   , (Point2 10 10, Point2 13 20)
                   , (Point2 12 10, Point2 20 5)
                   ]
testSegs2 :: [LineSegment 2 () R :+ ()]
testSegs2 = map (\(p,q) -> ClosedLineSegment (ext p) (ext q) :+ ())
                   [ (origin, Point2 10 0)
                   , (Point2 10 0, Point2 10 10)
                   , (origin, Point2 10 10)

                   , (origin, Point2 (-10) 0)
                   , (Point2 (-10) 0, Point2 (-10) (-10))
                   , (origin, Point2 (-10) (-10))
                   ]


--------------------------------------------------------------------------------

data Dummy

triangleSpec :: Spec
triangleSpec = describe "triangle fromAjRep" $ do
                 it "outerFace Correct" $ do
                   let i = outerFaceId myTriangle
                   myTriangle^.dataOf i `shouldBe` "OuterFace"


myTriangle :: PlaneGraph Dummy Int String String R
myTriangle = fromAdjRep myTriangle'

myTriangle' :: Gr (Vtx Int String R) (Face String)
myTriangle' = Gr [ Vtx 0 (Point2 0 0) [ (2,"0->2")
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

-- smallG = fromAdjRep (Proxy :: Proxy ()) small

-- small :: Gr (Vtx Int String Int) (Face String)
-- small = Gr [ Vtx 0 (Point2 0 0) [ (2,"0->2")
--                                 , (1,"0->1")
--                                 , (3,"0->3")
--                                 ] 0
--            , Vtx 1 (Point2 2 2) [ (0,"1->0")
--                                 , (2,"1->2")
--                                 , (3,"1->3")
--                                 ] 1
--            , Vtx 2 (Point2 2 0) [ (0,"2->0")
--                                 , (1,"2->1")
--                                 ] 2
--            , Vtx 3 (Point2 (-1) 4) [ (0,"3->0")
--                                    , (1,"3->1")
--                                    ] 3
--            ]
--            [ Face (2,1) "OuterFace"
--            , Face (0,1) "A"
--            , Face (1,0) "B"
--            ]

{-
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
-}

data InOrOut = In | Out deriving (Show,Eq)

simplePgGraph :: Num r => PlaneGraph Test1 () () InOrOut r
simplePgGraph = fromSimplePolygon @Test1 pg In Out
  where
    pg = unsafeFromPoints . map ext $ [ Point2 144 160
                                      , Point2 64 96
                                      , Point2 128 32
                                      , Point2 208 96
                                      ]



--------------------------------------------------------------------------------

-- | Tests if all face orientations are correct
allFaceOrientations    :: (Fractional r, Ord r) => PlaneGraph s v e f r -> Bool
allFaceOrientations pg = all (isCounterClockwise . (^._2.core)) ipgs
                      && isCounterClockwise (opg^.core)
  where
    ((_,opg),ipgs) = facePolygons (outerFaceId pg) pg



outerBoundaryVerticesTestSegs :: V.Vector (Point 2 R)
outerBoundaryVerticesTestSegs = V.fromList $
                                [ origin
                                , Point2 20 5
                                , Point2 12 10
                                , Point2 10 10
                                , Point2 13 20
                                ]


--------------------------------------------------------------------------------


-- | gets a dart incident to the outer face (in particular, that has the
-- outerface on its left)
--
-- running time: \(O(n)\)
--
outerFaceDartFractional    :: (Ord r, Fractional r) => PlaneGraph s v e f r -> Dart s
outerFaceDartFractional ps = d
  where
    (v,_)  = V.minimumBy (comparing (^._2.location)) . vertices $ ps
           -- compare lexicographically; i.e. if same x-coord prefer the one with the
           -- smallest y-coord
    d :+ _ = V.maximumBy (cmpSlope `on` (^.extra))
           .  fmap (\d' -> d' :+ edgeSegment d' ps ^. core.to supportingLine)
           $ incidentEdges v ps
    -- based on the approach sketched at https://cstheory.stackexchange.com/questions/27586/finding-outer-face-in-plane-graph-embedded-planar-graph
    -- basically: find the leftmost vertex, find the incident edge with the largest slope
    -- and take the face left of that edge. This is the outerface.
    -- note that this requires that the edges are straight line segments

      -- this implementation uses a fractional constraint, which I don't think we need.
