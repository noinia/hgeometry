{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Plane.BatchPointLocationSpec where

import Plane.BatchedPointLocation
import Test.Hspec
import HGeometry.Kernel
import Ipe
import System.OsPath
import Control.Lens
import HGeometry.Number.Real.Rational
import Test.Hspec.WithTempFile
import Data.Foldable
import Golden
import Data.List.NonEmpty(NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Line.BatchPointLocation qualified as Line

--------------------------------------------------------------------------------

type R = RealNumber 5


spec :: Spec
spec = describe "Plane.BatchedPointlocation" $ do
         goldenWith [osp|data/test-with-ipe/golden/Plane/|]
           (ipeFileGolden { name      = [osp|batchpointlocate|] }
           )
           ( let myLines'     = (iO . defIO) <$> myLines
                 queryPoints' = (iO . defIO) <$> queryPoints
                 answers      = Line.groupQueries queryPoints myLines
                 lr pts       = (iO . defIO) <$> pts
                 answers'     = [ iO $ ipeGroup (lr group) ! attr SLayer (LayerName (Text.show i))
                                | (i, group) <- zip [0..] (toList answers)
                                ]

                 content' = transformBy t $
                   [ iO $ ipeGroup myLines'     ! attr SLayer "lines"
                   , iO $ ipeGroup queryPoints' ! attr SLayer "queries"
                   ] <> answers'
                 t    = uniformScaling 10
             in addStyleSheet opacitiesStyle $ singlePageFromContent content'
           )

-- | fit to something that fits in this rectangle
screenBox :: Rectangle (Point 2 R)
screenBox = Rectangle origin (Point2 500 500)


myLines :: [VerticalOrLineEQ R]
myLines = [ NonVertical $ LineEQ 0 2
          , NonVertical $ LineEQ 1 3
          , NonVertical $ LineEQ (-1) 6
          -- , VerticalLineThrough 5
          ]

queryPoints :: NonEmpty (Point 2 R)
queryPoints = NonEmpty.fromList
              [ origin
              , Point2 1 8
              , Point2 (-1) 0
              , Point2 10 4
              , Point2 (-30) (-23)
              ]
