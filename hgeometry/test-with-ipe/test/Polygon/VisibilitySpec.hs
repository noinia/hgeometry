{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Polygon.VisibilitySpec where

import           Control.Lens
import           Data.Maybe
-- import qualified Data.Set as Set
import           Golden
import           Data.List.NonEmpty (NonEmpty(..))
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Polygon
import           HGeometry.Polygon.Instances ()
import           HGeometry.Polygon.Simple
-- import           HGeometry.Polygon.Visibility
import qualified HGeometry.Polygon.Visibility.Naive as Naive
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
-- import           Test.Hspec.QuickCheck
import           Test.Hspec.WithTempFile
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
-- import qualified Data.Text as Text


--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "visibility graph / visibility polygon" $ do
         -- prop "naive visibility graph and fast one the same " $
         --   \(pg :: SimplePolygon (Point 2 R)) ->
         --     Set.fromList (visibilityGraph pg) === Set.fromList (Naive.visibilityGraph pg)
         polygonPages <- runIO readInputPolygons
         goldenWith [osp|data/test-with-ipe/golden/Polygon/|]
           (ipeFileGolden { name = [osp|visibility|] })
             (ipeFile $ perPage <$> polygonPages)

readInputPolygons :: IO (NonEmpty (IpePage R))
readInputPolygons = do
  Right file <- readIpeFile [osp|data/test-with-ipe/golden/Polygon/visibilityInputPolygons.ipe|]
  pure $ file^.pages


perPage      :: IpePage R -> IpePage R
perPage page = fromContent . foldMap (drawSingle . (^.core)) $ readAll page

drawSingle    :: SimplePolygon (Point 2 R) -> [IpeObject R]
drawSingle pg = concat
  [ [ iO $ defIO (drawVisibilityEdge e pg) ! attr SStroke red
                                           ! attr SLayer "naive"

    | e <- Naive.visibilityGraph pg
    ]
             -- , [ iO $ defIO (drawVisibilityEdge e pg) ! attr SStroke red
             --                                          ! attr SLayer "visibilityGraph"

             --   | e <- visibilityGraph pg
             --   ]
  , [iO $ defIO pg ! attr SLayer "polygon" ]
  ]





drawVisibilityEdge :: Vector 2 Int -> SimplePolygon (Point 2 R) -> ClosedLineSegment (Point 2 R)
drawVisibilityEdge (Vector2 u v) pg = ClosedLineSegment (pg^?!vertexAt u) (pg^?!vertexAt v)

--------------------------------------------------------------------------------

myPolygon :: SimplePolygon (Point 2 R)
myPolygon = fromJust
          $ fromPoints [ origin
                       , Point2 100 100
                       , Point2 0 100
                       , Point2 (-20) 80
                       , Point2 (-20) 200
                       , Point2 (-30) 200
                       , Point2 (-30) (-20)
                       , Point2 (-25) 0
                       ]

-- out = pg

-- | Generates some input files
createFile :: IO ()
createFile = do
  pgs <- sample' arbitrary
  writeIpeFile [osp|visibilityInputPolygons.ipe|] $
                 ipeFile $ fmap (\pg -> fromContent [ iO $ defIO pg ]) (myPolygon :| pgs)
