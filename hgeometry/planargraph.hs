{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.PlanarGraph.Immutable
import qualified Data.PlanarGraph.Mutable as Mut

import           Control.Lens          ()
import           Control.Monad.ST
import           Data.Ext
import           Data.Foldable         as F
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.STRef
import           Data.Hashable
import qualified Data.Text             as T
import qualified Data.Vector           as V
import qualified Data.Vector.Circular  as CV
import           Graphics.SvgTree      (Number (..))
import           Linear.Metric
import           Linear.V2
import           Linear.Vector
import           Reanimate
import           Reanimate.Animation
import           System.Directory
import           System.FilePath
import Debug.Trace

graphs :: [PlanarGraph]
graphs =
  [ pgFromFaces [[0..2]]
  , pgFromFaces [[1,2,3]]
  , pgFromFaces [[0..3]]
  , pgFromFaces [[0..3],[4,3,2,1]]
  , let pg = pgFromFaces [[0..3]]
    in pgMutate pg $ \pg' -> do
          let he0 = Mut.halfEdgeFromId 0 pg'
              he4 = Mut.halfEdgeFromId 4 pg'
          _newEdge <- Mut.pgConnectVertices he0 he4
          return ()
  , pgFromFaces [[0,4,1],[0,1,2],[4,3,1],[4,5,3],[3,5,2],[2,5,0]]
  ]

main :: IO ()
-- main = reanimate $ staticFrame (1/60) (fst test2)
main = do
  forM_ graphs savePlanarGraphSVG

-- test1 = renderPlanarGraph (pgFromFaces [[0..2]])
-- test2 = renderPlanarGraph (pgFromFaces [[0..3],[4,3,2,1]])

savePlanarGraphSVG :: PlanarGraph -> IO ()
savePlanarGraphSVG pg = do
    writeFile fileName svgOutput
    writeFile compactName compactOutput
  where
    svgOutput = renderSvg (Just $ Num 300) (Just $ Num 300) svg
    compactOutput = renderSvg (Just $ Num 300) (Just $ Num 300) compactSvg
    fileName = "planargraph-" ++ show (hash pg) <.> "svg"
    compactName = "planargraph-" ++ show (hash pg) <.> "compact" <.> "svg"
    defOpts = RenderOptions { disableHalfEdges = False }
    compactOpts = RenderOptions { disableHalfEdges = True }
    svg = renderPlanarGraph defOpts pg
    compactSvg = renderPlanarGraph compactOpts pg

data RenderOptions = RenderOptions
  { disableHalfEdges :: Bool }

renderPlanarGraph :: RenderOptions -> PlanarGraph -> SVG
renderPlanarGraph RenderOptions{..} pg = svg
  where
    vs = tutteEmbedding pg
    faces = pgFaces pg
    svg =
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
          | face <- faces
          , let boundary = faceBoundary face pg
                poly = simpleFromPoints $ map ext
                        [ Point2 x y
                        | vId <- boundary
                        , let V2 x y = vs V.! vertexId vId
                        ]
                Point2 x y = centroid poly
                label = scale 0.5 $ center $ latex (T.pack $ show $ faceId face)
          ]
        , mkGroup
          [ mkGroup $
            [ withStrokeColor "black" $
              mkLine (tipX*scaleFactor, tipY*scaleFactor) (tailX*scaleFactor, tailY*scaleFactor)
            ] ++ if disableHalfEdges
              then []
              else 
                [ translate (halfX*scaleFactor + angY) (halfY*scaleFactor - angX) $
                  labelTwin
                , translate (halfX*scaleFactor - angY) (halfY*scaleFactor + angX) $
                  labelEdge
                ]
          | edge <- pgEdges pg
          , let (tip, tail) = edgeHalfEdges edge
                V2 tipX tipY = vs V.! vertexId (halfEdgeVertex tip pg)
                V2 tailX tailY = vs V.! vertexId (halfEdgeVertex tail pg)
                (halfX,halfY) = (tipX + (tailX-tipX)/2, tipY + (tailY-tipY)/2)
                labelEdge = scale 0.4 $ center $ latex (T.pack $ show $ halfEdgeId tip)
                labelTwin = scale 0.4 $ center $ latex (T.pack $ show $ halfEdgeId tail)
                V2 angX angY = signorm (V2 tipX tipY - V2 tailX tailY) ^* 0.3
          ]
        , mkGroup
          [ translate (x*scaleFactor) (y*scaleFactor) $
            mkGroup
            [ withFillOpacity 1 $ withFillColor "white" $ withStrokeColor "black" $
              mkCircle 0.3
            , label ]
          | v <- map vertexId $ pgVertices pg
          , let V2 x y = vs V.! v
                label = scaleToWidth 0.2 $ center $ latex (T.pack $ show v)
          ]
        ]

strokeWidth = defaultStrokeWidth*0.5

bgColor :: String
bgColor = "white"

scaleFactor :: Double
scaleFactor = screenTop*0.8
