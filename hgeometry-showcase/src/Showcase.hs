module Main (main) where

import System.Environment (getArgs, withArgs)
import System.Exit        (ExitCode (ExitFailure), exitWith)
import System.IO          (hPutStrLn, stderr)

import ClosestPair (closestPairShowcase)
import Common
import ConvexHull  (convexHullShowcase)

import Reanimate

main :: IO ()
main = do
  args <- getArgs
  case args of
    "convexhull":rest  -> withArgs rest $ runReanimate convexHullShowcase
    "closestpair":rest -> withArgs rest $ runReanimate closestPairShowcase
    _                  -> printUsage

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
