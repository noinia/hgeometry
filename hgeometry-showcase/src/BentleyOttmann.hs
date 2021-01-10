{-# LANGUAGE DataKinds #-}
module BentleyOttmann where

import Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann

import           Control.Lens
import           Control.Monad.Random
import           Data.Ext
import Data.Hashable
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Convex
import qualified Data.List.NonEmpty           as NonEmpty
import qualified Data.Vector.Circular         as CV
import           Reanimate
import           Graphics.SvgTree          (LineJoin (..), Cap(..), strokeLineCap)
import qualified Data.Map as Map

import Common

-- intersections    :: (Ord r, Fractional r)
--                  => [LineSegment 2 p r] -> Intersections p r

bentleyOttmannShowcase :: Animation
bentleyOttmannShowcase = scene $ do
  forM_ [0 .. length lineSegments-1] $ \i -> do
    let this = CV.index lineSegments i
        next = CV.index lineSegments (i+1)
    play $ animateTransition (zip this next)
      & signalA (curveS 2)
      & setDuration 2
      & pauseAtEnd 1

animateTransition :: [(LineSegment 2 () Rational, LineSegment 2 () Rational)] -> Animation
animateTransition pairs = animate $ \t ->
  let ls = [ lerpLineSegment t b a | (a,b) <- pairs ]
  in mkGroup
    [ showLines ls
    -- , showPoints ls
    , showIntersections ls
    ]

showIntersections :: [LineSegment 2 () Rational] -> SVG
showIntersections ls = mkGroup
    [ ppMarker p
    | p <- Map.keys int
    ]
  where
    int = intersections ls

-- type Intersections p r = Map.Map (Point 2 r) (Associated p r)
-- data Associated p r = Associated { _endPointOf        :: Set' (LineSegment 2 p r)
--                                  , _interiorTo        :: Set' (LineSegment 2 p r)
--                                  } deriving (Show, Generic)

showLines :: [LineSegment 2 () Rational] -> SVG
showLines ls =
  (strokeLineCap .~ pure CapRound) $
  withStrokeColorPixel black $ mkGroup
  [ mkLine (x1,y1) (x2,y2)
  | LineSegment' a b <- ls
  , let Point2 x1 y1 = realToFrac <$> a^.core
        Point2 x2 y2 = realToFrac <$> b^.core
  ]

showPoints :: [LineSegment 2 () Rational] -> SVG
showPoints ls = mkGroup
  [ mkGroup [ppPoint (a^.core), ppPoint (b^.core) ]
  | LineSegment' a b <- ls
  ]

seed :: Int
seed = hash "bentleyottman"

nSets :: Int
nSets = 10

nLines :: Int
nLines = 8

lineSegments :: CV.CircularVector [LineSegment 2 () Rational]
lineSegments = CV.unsafeFromList $ map (map $ scaleLineSegment 0.9) $ flip evalRand (mkStdGen seed) $
  replicateM nSets (replicateM nLines genLineSegment)

ppPoint :: Real r => Point 2 r -> SVG
ppPoint (Point2 x y) =
  withFillColorPixel green $
  withStrokeColorPixel black $
  translate (realToFrac x) (realToFrac y) $
  mkCircle nodeRadius

ppMarker :: Real r => Point 2 r -> SVG
ppMarker (Point2 x y) =
  withFillColorPixel red $
  withStrokeColorPixel black $
  translate (realToFrac x) (realToFrac y) $
  mkCircle (nodeRadius*0.5)

