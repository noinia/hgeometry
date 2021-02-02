{-# LANGUAGE RankNTypes #-}
module Main where

import Data.PlanarGraph.Mutable

import           Control.Monad.ST
import           Control.Lens ()
import           Data.STRef
import qualified Data.Text            as T
import qualified Data.Vector          as V
import qualified Data.Vector.Circular as CV
import           Reanimate
import           Reanimate.Animation
import           Graphics.SvgTree (Number(..))
import Data.Geometry.Polygon
import Data.Geometry.Point
import Data.Ext
import Data.Foldable as F
import Linear.V2
import Linear.Vector
import Linear.Metric
import System.Directory
import System.FilePath

main :: IO ()
-- main = reanimate $ staticFrame (1/60) (fst test2)
main = do
  savePlanarGraphSVG $ pgFromFaces [[0..2]]
  savePlanarGraphSVG $ pgFromFaces [[0..3]]
  savePlanarGraphSVG $ pgFromFaces [[0..3],[4,3,2,1]]

test1 = renderPlanarGraph (pgFromFaces [[0..2]])
test2 = renderPlanarGraph (pgFromFaces [[0..3],[4,3,2,1]])

savePlanarGraphSVG :: (forall s. ST s (PlanarGraph s)) -> IO ()
savePlanarGraphSVG genPG = do
    writeFile fileName svgOutput
  where
    svgOutput = renderSvg (Just $ Num 300) (Just $ Num 300) svg
    fileName = "planargraph-" ++ show (abs hashValue) <.> "svg"
    (svg, hashValue) = renderPlanarGraph genPG

renderPlanarGraph :: (forall s. ST s (PlanarGraph s)) -> (SVG, Int)
renderPlanarGraph genPG= runST $ do
  pg <- genPG
  h <- pgHash pg
  vs <- tutteEmbedding pg
  maxEdgeId <- readSTRef (pgNextHalfEdgeId pg)
  edgeVertex <- V.freeze =<< readSTRef (pgHalfEdgeVertex pg)
  faceCount <- readSTRef (pgNextFaceId pg)
  faces <- mapM (\f -> CV.map vertexToId <$> faceBoundary f) [faceFromId fId pg | fId <- [0.. faceCount-1]]
  let svg =
        withViewBox (screenBottom, screenBottom, screenHeight, screenHeight) $
        mkGroup
        [ mkBackground bgColor
        , mkGroup
          [ translate (x*scaleFactor) (y*scaleFactor) $
            mkGroup
            [ label
            , withFillOpacity 0 $ withStrokeColor "black" $ withStrokeWidth strokeWidth $
              -- mkRect (strokeWidth+svgWidth label*1.5) (strokeWidth+svgHeight label*1.5)
              translate 0 (-(svgHeight label+0.1)/2) $
              center $ mkLine (0,0) (svgWidth label*1.2,0)
            ]
          | (fId, face) <- zip [0..] faces
          , let poly = simpleFromPoints $ map ext
                        [ Point2 x y
                        | vId <- CV.toList face
                        , let (x,y) = vs V.! vId
                        ]
                Point2 x y = centroid poly
                label = scale 0.5 $ center $ latex (T.pack $ show fId)
          ]
        , mkGroup
          [ mkGroup
            [ withStrokeColor "black" $
              mkLine (tipX*scaleFactor, tipY*scaleFactor) (tailX*scaleFactor, tailY*scaleFactor)
            , translate (halfX*scaleFactor + angY) (halfY*scaleFactor - angX) $
              mkGroup
              [labelTwin
              ]
            , translate (halfX*scaleFactor - angY) (halfY*scaleFactor + angX) $
              mkGroup
              [ labelEdge
              ]
            ]
          | edge <- [0 .. maxEdgeId-1]
          , let tip = edgeVertex V.! (edge*2)
                tail = edgeVertex V.! (edge*2+1)
                (tipX,tipY) = vs V.! tip
                (tailX,tailY) = vs V.! tail
                (halfX,halfY) = (tipX + (tailX-tipX)/2, tipY + (tailY-tipY)/2)
                labelEdge = scale 0.4 $ center $ latex (T.pack $ show (edge*2))
                labelTwin = scale 0.4 $ center $ latex (T.pack $ show (edge*2+1))
                V2 angX angY = signorm (V2 tipX tipY - V2 tailX tailY) ^* 0.3
          ]
        , mkGroup
          [ translate (x*scaleFactor) (y*scaleFactor) $
            mkGroup
            [ withFillOpacity 1 $ withFillColor "white" $ withStrokeColor "black" $
              mkCircle 0.3
            , label ]
          | n <- [0..V.length vs-1]
          , let (x,y) = vs V.! n
                label = scaleToWidth 0.2 $ center $ latex (T.pack $ show n)
          ]
        ]
  return (svg, h)

strokeWidth = defaultStrokeWidth*0.5

bgColor :: String
bgColor = "white"

scaleFactor :: Double
scaleFactor = screenTop*0.8
