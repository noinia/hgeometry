module Main (main) where

import System.Environment (getArgs, withArgs)
import System.Exit        (ExitCode (ExitFailure), exitWith)
import System.IO          (hPutStrLn, stderr)

import ClosestPair (closestPairShowcase)
import Common
import ConvexHull  (convexHullShowcase)
import BentleyOttmann
import FastVisibility
import SSSP
import RandomMonotone
import ZHashing

import Reanimate

main :: IO ()
main = do
  args <- getArgs
  case args of
    "sssp":rest            -> withArgs rest $ runReanimate ssspMulti
    "convexhull":rest      -> withArgs rest $ runReanimate convexHullShowcase
    "closestpair":rest     -> withArgs rest $ runReanimate closestPairShowcase
    "bentleyottmann":rest  -> withArgs rest $ runReanimate bentleyOttmannShowcase
    "fast_visibility":rest -> withArgs rest $ runReanimate fastVisibilityShowcase
    "random_monotone":rest -> withArgs rest $ runReanimate randomMonotoneShowcase
    "zhashing":rest        -> withArgs rest $ runReanimate zHashingShowcase
    _                      -> printUsage

runReanimate :: Animation -> IO ()
runReanimate animation = reanimate $
  mapA (withViewBox (screenBottom, screenBottom, screenHeight, screenHeight)) $
  scene $ do
    newSpriteSVG_ $ mkBackground bgColor
    play animation

printUsage :: IO ()
printUsage = do
  hPutStrLn stderr "usage"
  exitWith (ExitFailure 1)
