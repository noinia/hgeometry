module Main where

import Algorithms.Geometry.ConvexHull.Minimalist (lowerHull')
import Data.Bifunctor
import Control.Lens
import Data.Ext
import Data.RealNumber.Rational
import Generate
import PLY.Writer


type R = RealNumber 5

main :: IO ()
main = do pts <- genPoints @R 2000
          let lh = lowerHull' pts
          renderOutputToFile "lowerHull.ply" (over core (fmap toDouble) <$> pts)
                                             (second toDouble <$> lh)
  where
    toDouble = realToFrac @R @Double
