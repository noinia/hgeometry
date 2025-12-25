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
import Golden

--------------------------------------------------------------------------------

type R = RealNumber 5


spec :: Spec
spec = describe "Plane.BatchedPointlocation" $ do
         goldenWith [osp|data/test-with-ipe/golden/Plane/|]
           (ipeFileGolden { name      = [osp|batchpointlocate|] }
           )
           ( let myLines'     = (iO . defIO) <$> myLines
                 queryPoints' = (iO . defIO) <$> queryPoints
                 content' =
                   [ iO $ ipeGroup myLines'     ! attr SLayer "lines"
                   , iO $ ipeGroup queryPoints' ! attr SLayer "queries"
                   ]
             in addStyleSheet opacitiesStyle $ singlePageFromContent content'
           )


myLines :: [VerticalOrLineEQ R]
myLines = [ NonVertical $ LineEQ 0 2
          , NonVertical $ LineEQ 1 3
          , NonVertical $ LineEQ (-1) 6
          , VerticalLineThrough 5
          ]

queryPoints :: [Point 2 R]
queryPoints = [ origin
              , Point2 1 5
              , Point2 (-1) 0
              ]
