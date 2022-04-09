module Main where

import Algorithms.Geometry.ConvexHull.Minimalist (lowerHull')
import Control.Lens
import Data.Bifunctor
import Data.Ext
import Data.RealNumber.Rational
import Generate
import PLY.Writer
import System.Environment(getArgs)


type R = RealNumber 5

main :: IO ()
main = do [n] <- fmap read <$> getArgs
          pts <- genPoints @R n
          let lh = lowerHull' pts
          renderOutputToFile "lowerHull.ply" (over core (fmap toDouble) <$> pts)
                                             (second toDouble <$> lh)
  where
    toDouble = realToFrac @R @Double
