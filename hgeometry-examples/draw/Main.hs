module Main(main) where

import Data.Proxy
import HGeometry.Instances ()
import HGeometry.Number.Real.Rational
import HGeometry.PlaneGraph
import HGeometry.PlaneGraph.Instances
import HGeometry.Point
import Test.QuickCheck

--------------------------------------------------------------------------------

type R = RealNumber 5


main :: IO ()
main = do
  print "go"
  (gr :: PlaneGraph QuickCheckWorld (Point 2 R) () ()) <- generate arbitrary
  print gr

  -- (grs :: [PlaneGraph QuickCheckWorld (Point 2 R) () ()]) <- sample' arbitrary
  -- mapM_ print grs
