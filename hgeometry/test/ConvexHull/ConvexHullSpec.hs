module ConvexHull.ConvexHullSpec where

import qualified HGeometry.ConvexHull.DivideAndConquer as DivideAndConquer
import qualified HGeometry.ConvexHull.GrahamScan as GrahamScan
-- import qualified Algorithms.Geometry.ConvexHull.JarvisMarch as JarvisMarch
-- import qualified Algorithms.Geometry.ConvexHull.OldDivideAndConquer as OldDivAndConquer
import qualified HGeometry.ConvexHull.QuickHull as QuickHull
import           Control.Lens
import           HGeometry.Ext
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Point
import           HGeometry.Cyclic
import           Hiraffe.Graph
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Convex
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           HGeometry.Instances ()
import           HGeometry.Number.Real.Rational

--------------------------------------------------------------------------------

type R = RealNumber 5

newtype PointSet = PS (NonEmpty (Point 2 R))
  deriving newtype (Show,Eq)

instance Arbitrary PointSet where
  arbitrary = do p <- arbitrary
                 q <- arbitrary `suchThat` (/= p)
                 r <- arbitrary `suchThat` (\r' -> r' /= p && r' /= q)
                 (\pts' -> PS $ NonEmpty.fromList $ [p,q,r] <> pts') <$> arbitrary

spec :: Spec
spec = do
    describe "ConvexHull Algorithms" $ do
      modifyMaxSize (const 1000) $ do
        describe "GrahamScan and DivideAnd Conquer are the same" $ do
          it "quickcheck convex hull " $
            property $ \(PS pts) ->
              (PG $ GrahamScan.convexHull pts) == (PG $ DivideAndConquer.convexHull pts)
          it "quickcheck upper hull " $
            property $ \(pts :: NonEmpty (Point 2 Int)) ->
              GrahamScan.upperHull pts == DivideAndConquer.upperHull pts
          it "quickcheck lower hull " $
            property $ \(pts :: NonEmpty (Point 2 Int)) ->
              GrahamScan.lowerHull pts == DivideAndConquer.lowerHull pts
          it "manual" $
            (PG $ GrahamScan.convexHull myPoints) == (PG $ DivideAndConquer.convexHull myPoints)

        -- describe "GrahamScan and Old DivideAnd Conquer are the same" $ do
        --   -- it "quickcheck " $
        --   --   property $ \pts ->
        --   --     (PG $ GrahamScan.convexHull pts) == (PG $ DivideAndConquer.convexHull pts)
        --   it "manual" $
        --     (PG $ GrahamScan.convexHull myPoints) == (PG $ OldDivAndConquer.convexHull myPoints)

        it "GrahamScan and QuickHull are the same" $
          property $ \(PS pts) ->
            (PG $ GrahamScan.convexHull pts) == (PG $ QuickHull.convexHull pts)

        -- it "JarvisMarch Manual test1" $
        --   JarvisMarch.convexHull testPoints
        --   `shouldBe`
        --   fromPointsUnsafe [mPoint2 [0,10],mPoint2 [2,20],mPoint2 [6,20]
        --                    ,mPoint2 [8,11],mPoint2 [8,6],mPoint2 [7,4]
        --                    ,mPoint2 [5,3],mPoint2 [1,4],mPoint2 [0,5]]

        -- it "JarvisMarch Manual test2" $
        --   JarvisMarch.convexHull testPoints2
        --   `shouldBe`
        --   fromPointsUnsafe [ mPoint2 [0,10], mPoint2 [2,20]
        --                    , mPoint2 [6,20], mPoint2 [8,11]
        --                    , mPoint2 [7,4],  mPoint2 [5,3] , mPoint2 [1,4] ]


        -- it "GrahamScan and JarvisMarch are the same" $
        --   property $ \pts ->
        --     (PG $ GrahamScan.convexHull pts) == (PG $ JarvisMarch.convexHull pts)






newtype PG = PG (ConvexPolygon (Point 2 R)) deriving (Show)

instance Eq PG where
  (PG a) == (PG b) = let as = toCyclic . toSimplePolygon $ a
                         bs = toCyclic . toSimplePolygon $ b
                     in isShiftOf as bs

myPoints :: NonEmpty.NonEmpty (Point 2 R)
myPoints = NonEmpty.fromList $
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


mPoint2 ~[x,y] = Point2 x y

testPoints = NonEmpty.fromList
  [ mPoint2 [0, 10]
  , mPoint2 [0, 5]
  , mPoint2 [1, 5]
  , mPoint2 [1, 4]
  , mPoint2 [5, 3]
  , mPoint2 [7, 4]
  , mPoint2 [8, 6]
  , mPoint2 [8, 11]
  , mPoint2 [6, 20]
  , mPoint2 [2, 20]
  ]

testPoints2 = NonEmpty.fromList
  [ mPoint2 [0, 10]
  , mPoint2 [1, 5]
  , mPoint2 [1, 4]
  , mPoint2 [5, 3]
  , mPoint2 [7, 4]
  , mPoint2 [8, 11]
  , mPoint2 [6, 20]
  , mPoint2 [2, 20]
  ]
