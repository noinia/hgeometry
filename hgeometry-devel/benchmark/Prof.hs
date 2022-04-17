module Main where

import Algorithms.Geometry.ConvexHull.Minimalist (lowerHull')
import Control.Lens
import Data.Aeson (decodeFileStrict')
import Data.Bifunctor
import Data.Ext
import Geometry.Point
import Data.List.NonEmpty (NonEmpty)
import Data.RealNumber.Rational
import PLY.Writer
import System.Environment (getArgs)

type R = RealNumber 5

main :: IO ()
main = do [fp] <- getArgs
          Just (pts :: NonEmpty (Point 3 R :+ Int)) <- decodeFileStrict' fp
          let lh = lowerHull' pts
          renderOutputToFile "lowerHull.ply" (over core (fmap toDouble) <$> pts)
                                             (second toDouble <$> lh)
  where
    toDouble = realToFrac @R @Double
