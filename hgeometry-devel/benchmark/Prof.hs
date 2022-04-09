module Main where

import qualified Algorithms.Geometry.ConvexHull.Minimalist as Minimalist
import           Data.RealNumber.Rational
import           Generate

type R = RealNumber 5

main :: IO ()
main = do pts <- genPoints @R 2000
          print $ Minimalist.lowerHull pts
