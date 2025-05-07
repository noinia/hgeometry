{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}
module DelaunayTriangulation.DTSpec (spec) where

import           Control.Lens
import           Data.Coerce
import           Data.Function (on)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe (fromJust, mapMaybe)
import           Data.Singletons (Apply)
import           Data.Vinyl
import           HGeometry
import qualified HGeometry.CircularList.Util as CU
import           Hiraffe.PlanarGraph.Connected (VertexIdIn(..))
-- import qualified HGeometry.DelaunayTriangulation.DivideAndConquer as DC
  -- FIXME: use DC
import qualified HGeometry.DelaunayTriangulation.Naive as DC
import qualified HGeometry.DelaunayTriangulation.Naive as Naive
import           HGeometry.DelaunayTriangulation
import           HGeometry.Ext
import           Ipe
import qualified Data.Vector as V
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust, mapMaybe)
import qualified HGeometry.PlaneGraph as PG
import           HGeometry.PlaneGraph
import           HGeometry.Number.Real.Rational
import qualified Data.Vector as V
import           Test.Hspec
import           Test.Util
import           System.OsPath
import           Golden
-- import           HGeometry.PlanarSubdivision
--------------------------------------------------------------------------------

type R = RealNumber 5


dtEdges :: (Fractional r, Ord r)
        => NonEmpty.NonEmpty (Point 2 r :+ p) -> [(VertexID, VertexID)]
dtEdges = edgesAsVertices . DC.delaunayTriangulation

take'   :: Int -> NonEmpty.NonEmpty a -> NonEmpty.NonEmpty a
take' i = NonEmpty.fromList . NonEmpty.take i

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Testing Divide and Conquer Algorithm for Delaunay Triangulation" $ do
    it "singleton " $ do
      dtEdges (take' 1 myPoints) `shouldBe` []
    toSpec (TestCase "myPoints" myPoints)
    toSpec (TestCase "myPoints'" myPoints')
    it "testing edges of 9 random pts" $
      fmap (\d -> trianG^.endPointsOf d.asIndex) (trianG^..darts.asIndex) == edgesPts''
    -- toSpec (TestCase "maartens points" buggyPoints3)

    ipeSpec

ipeSpec :: Spec
ipeSpec = testCases [osp|test-with-ipe/Disk/minDisk.ipe|]

testCases    :: OsPath -> Spec
testCases fp = (runIO $ readInput =<< getDataFileName fp) >>= \case
    Left e    -> it "reading Delaunay Triangulation disk file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


-- | Point sets per color, Crosses form the solution
readInput    :: OsPath -> IO (Either ConversionError [TestCase () R])
readInput fp = fmap f <$> readSinglePageFile fp
  where
    f page = [ TestCase "?" $ fmap (\p -> p^.core.symbolPoint :+ ()) pSet
             | pSet <- byStrokeColour' syms
             ]
      where
        syms = page^..content.traverse._IpeUse
        byStrokeColour' = mapMaybe NonEmpty.nonEmpty . byStrokeColour


data TestCase p r = TestCase { _color    :: String
                             , _pointSet :: NonEmpty.NonEmpty (Point 2 r :+ p)
                             } deriving (Show,Eq)


toSpec                    :: (Fractional r, Ord r, Show r, Show p) => TestCase p r -> Spec
toSpec (TestCase c pts) = describe ("testing on " ++ c ++ " points") $ do
                            sameAsNaive c pts

sameAsNaive       :: (Fractional r, Ord r, Show p, Show r)
                  => String -> NonEmpty.NonEmpty (Point 2 r :+ p) -> Spec
sameAsNaive s pts = it ("Divide And Conquer same answer as Naive on " ++ s) $
                      (Naive.delaunayTriangulation pts
                       `sameEdges`
                       DC.delaunayTriangulation pts) `shouldBe` True


sameEdges             :: Triangulation p r -> Triangulation p r -> Bool
triA `sameEdges` triB = all sameAdj . Map.assocs $ mapping'
  where
    sameAdj (a, b) = (f $ adjA V.! a) `CU.isShiftOf` (adjB V.! b)

    adjA = triA^.neighbours
    adjB = triB^.neighbours

    mapping' = Map.fromList $ zip (Map.elems $ triA^.vertexIds) (Map.elems $ triB^.vertexIds)

    f = fmap (fromJust . flip Map.lookup mapping')

myPoints :: NonEmpty.NonEmpty (Point 2 R :+ ())
myPoints = NonEmpty.fromList . map ext $
           [ Point2 1  3
           , Point2 4  26
           , Point2 5  17
           , Point2 6  7
           , Point2 12 16
           , Point2 19 4
           , Point2 20 0
           , Point2 20 11
           , Point2 23 23
           , Point2 31 14
           , Point2 33 5
           ]

myPoints' :: NonEmpty.NonEmpty (Point 2 R :+ ())
myPoints' = NonEmpty.fromList . map ext $
            [ Point2 64  736
            , Point2 96 688
            , Point2 128 752
            , Point2 160 704
            , Point2 128 672
            , Point2 64 656
            , Point2 192 736
            , Point2 208 704
            , Point2 192 672
            ]

--------------------------------------------------------------------------------

type GR = PlaneGraph () (Point 2 R :+ ()) () ()

trianG = let dts    = DC.delaunayTriangulation pts''
         in toPlaneGraph @() dts

pts'' :: NonEmpty.NonEmpty (Point 2 R :+ Int)
pts'' = read "(Point2 (-214) 142 :+ 0) :| [Point2 (-59) 297 :+ 2,Point2 (-135) 141 :+ 1,Point2 193 (-123) :+ 6,Point2 51 (-147) :+ 3,Point2 242 114 :+ 7,Point2 186 290 :+ 5,Point2 262 293 :+ 8,Point2 109 1 :+ 4]"

edgesPts'' :: [(VertexIx GR, VertexIx GR)]
edgesPts'' = let f = coerce @Int
             in bimap f f <$>
  [(0,3),(0,1),(0,2),(1,0),(1,3),(1,4),(1,2),(2,0),(2,1),(2,4),(2,5),(2,8),(3,6),(3,4),(3,1),(3,0),(4,6),(4,7),(4,5),(4,2),(4,1),(4,3),(5,2),(5,4),(5,7),(5,8),(6,7),(6,4),(6,3),(7,8),(7,5),(7,4),(7,6),(8,2),(8,5),(8,7)]



--------------------------------------------------------------------------------
-- Issue #28 mentions that these sets loop.

-- for Doubles I guess.
{-

buggyPoints :: NonEmpty.NonEmpty (Point 2 (RealNumber 10) :+ Int)
buggyPoints = NonEmpty.fromList $ [ Point2 38.5 3.5  :+ 0
                                  , Point2 67.0 33.0 :+ 1
                                  , Point2 46.0 45.5 :+ 2
                                  , Point2 55.5 42.0 :+ 3
                                  , Point2 36.0 25.0 :+ 4
                                  , Point2 76.5 12.0 :+ 5
                                  , Point2 29.0 26.5 :+ 6
                                  , Point2 55.0 10.5 :+ 7
                                  ]

buggyPoints2 :: NonEmpty.NonEmpty (Point 2 (RealNumber 18) :+ Char)
buggyPoints2 = NonEmpty.fromList $ [ Point2 217.44781269876754 249.24741543498274 :+ 'a'
                                   , Point2 237.91428506927295 105.8082929316906  :+ 'b'
                                   , Point2 51.46936876163245 193.21960885915342  :+ 'c'
                                   , Point2 172.55365082143922 2.8346743864823387 :+ 'd'
                                   , Point2 250.55083565080437 93.13205719006257  :+ 'e'
                                   ]


-- | Maarten reported a problem with the EMST of this set
buggyPoints3 :: NonEmpty.NonEmpty (Point 2 (RealNumber 18) :+ Int)
buggyPoints3 = NonEmpty.fromList
             $ [ Point2 (-128) (-16)                                        :+ 1
               , Point2 (-64) (-80)                                         :+ 2
               , Point2 (-2097151243 / 32768000) (-2621440757 / 32768000)   :+ 3
               , Point2 (-16) (-128)                                        :+ 4
               , Point2 64 96                                               :+ 5
               ]
-}


--------------------------------------------------------------------------------
-- From bug #149

-- pts' :: IO (NonEmpty.NonEmpty (Point 2 R :+ Int))
-- pts' = do let pointCount = 9
--           rvs <- replicateM pointCount (randomRIO (Point2 (-300) (-300), Point2 300 300))
--           let ne = NonEmpty.fromList $ zipWith (:+) (fmap (fromIntegral @Int @R) <$> rvs) [0..]
--           pure ne

-- withEndPoints pg d = let (VertexData u _, VertexData v _) = pg^.PG.endPointsOf d
--                      in (d, (u,v))


-- fooPG ne = do let dts    = DC.delaunayTriangulation ne
--                   trian = toPlaneGraph (Proxy @()) dts
--               mapM_ print ne
--               print "============================"
--               print trian
--               print "============================"
--               let oid = PG.outerFaceId trian
--               mapM_ print $ fmap (withEndPoints trian) $ PG.boundary oid trian
--               print "============================"
--               -- mapM_ print $ trian^.rawFaceData
--               -- print "============================"
--               -- mapM_ print $ PG.boundaryVertices oid trian
--               -- print "============================"
--               -- mapM_ print $ snd (PG.facePolygons (PG.outerFaceId trian) trian)
--               -- printAsIpeSelection . iO . defIO . view core
--               --   $ outerFacePolygon (PG.outerFaceId trian) trian

--               printAsIpeSelection $ drawPlaneGraph' trian

--               -- printAs $ fmap snd . PG.edgeSegments $ trian

-- foo ne = do let dts    = DC.delaunayTriangulation ne
--                 trian = toPlanarSubdivision (Proxy @()) $ dts
--             mapM_ print ne
--             print "============================"
--             print dts
--             print "============================"
--             mapM_ print $ trian^.rawFaceData
--             print "============================"
--             print $ PG.boundaryVertices (FaceId $ VertexId 7) $ trian^.components.to V.head
--             print "============================"
--             mapM_ print (internalFacePolygons trian)

-- -- internalFaces' = V.tail . faces'


byStrokeColour :: (Stroke ∈ ats, Ord (Apply f Stroke))
               => [a :+ Attributes f ats] -> [[a :+ Attributes f ats]]
byStrokeColour = map (map fst) . List.groupBy ((==) `on` snd) . List.sortOn snd
               . map (\x -> (x,lookup' x))
  where
    lookup' (_ :+ ats) = lookupAttr SStroke ats
