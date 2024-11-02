module Main(main) where

import HGeometry.PlaneGraph.Instances
import Test.QuickCheck
import Data.Proxy
import HGeometry.Number.Real.Rational
import HGeometry.PlaneGraph
import HGeometry.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5


main = do
  (grs :: [PlaneGraph QuickCheckWorld (Point 2 R) () ()]) <- sample' arbitrary
  mapM_ print grs
