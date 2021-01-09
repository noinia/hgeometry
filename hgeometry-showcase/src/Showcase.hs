module Main where

import ConvexHull
import Common

import Reanimate

main :: IO ()
main = reanimate $
  mapA (withViewBox (screenBottom, screenBottom, screenHeight, screenHeight)) $
  scene $ do
    newSpriteSVG_ $ mkBackground bgColor
    play convexHullShowcase
