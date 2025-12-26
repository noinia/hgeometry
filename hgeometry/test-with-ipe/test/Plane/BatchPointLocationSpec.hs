{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Plane.BatchPointLocationSpec where

import Data.Default
import Plane.BatchedPointLocation
import Test.Hspec
import HGeometry.Kernel
import Ipe
import System.OsPath
import Control.Lens
import HGeometry.Ext
import HGeometry.Number.Real.Rational
import HGeometry.PlaneGraph.Connected
import Test.Hspec.WithTempFile
import Data.Foldable
import Golden
import Data.List.NonEmpty(NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Line.BatchPointLocation qualified as Line
import PlaneGraph.PolygonOverlaySpec
import PlaneGraph.RenderSpec

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
                 subdiv = Line.buildPointLocationStructure queryPoints myLines

                 gr :: CPlaneGraph () (Point 2 R) (ClosedLineSegment (Point 2 R) :+ Maybe (VerticalOrLineEQ R)) String
                 gr = (subdiv^.Line.subdivision) & faces %@~ \i _ -> show i
                 faces' = ifoldMapOf interiorFacePolygons (drawFace gr) gr
                 content' = transformBy t $
                   [ iO $ ipeGroup myLines'     ! attr SLayer "lines"
                   , iO $ ipeGroup queryPoints' ! attr SLayer "queries"
                   , iO $ ipeGroup (renderGraph (renderSubdiv subdiv)) ! attr SLayer "subdiv"
                   ] <> faces' <> answers'
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


--------------------------------------------------------------------------------

renderSubdiv    :: forall r line.
                     Line.PointLocationDS' r line
                  -> CPlaneGraph () (Point 2 r)
                                    (Maybe (IpeAttributes Path R))
                                    (Maybe (IpeAttributes Path R))
renderSubdiv ds = gr1&faces ?~ def
  where
    gr = ds^.Line.subdivision
    gr1 :: CPlaneGraph () (Point 2 r)
                          (Maybe (IpeAttributes Path R))
                          ()
    gr1 = gr&edges ?~ def
      -- \(E defs covering) -> do
      --                     _ :+ (_ :+ (z,props)) <- minimumOn (^.extra.extra._1) defs
      --                     _ :+ (zCovering,_)    <- minimumOn (^.extra._1) covering
      --                     if z <= zCovering then props^.edgeAttrs
      --                                       else Nothing
