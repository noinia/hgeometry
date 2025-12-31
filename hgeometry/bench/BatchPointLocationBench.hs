module Main( main ) where

import Data.Maybe(mapMaybe)
import Control.DeepSeq
import Data.List.NonEmpty (NonEmpty(..), toList)
import HGeometry.Plane.BatchPointLocation
import HGeometry.Line.BatchPointLocation qualified as Line
import Prelude
import HGeometry.Plane
import HGeometry.Point
import HGeometry.Box
import HGeometry.Number.Real.Rational

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

main :: IO ()
main = do
    let planes :: NonEmpty (Plane (RealNumber 5))
        planes = Plane (-29.00667) 13.49623 474.06827 :| [Plane 3.47711 (-4.57120) (-25.68051),Plane (-28.82754) 19.91868 506.94435]
        queries = Point3 43.33479 60.20311 29.58576 :| [Point3 16.10390 1.64560 29.15703,Point3 19.25262 11.50219 70.84993]
        myLines = mapMaybe (uncurry projectedIntersectionLine)
                  [ (x,y) | x <- toList planes, y <- toList planes ]
        ds      = Line.pointLocationStructureIn (Rect (-1) (-1) 128 128) myLines

    -- ds `seq` (pure ()) -- uncommenting this makes the <<<loop>>> go away
    batchedPointLocation queries planes `deepseq` (pure ())
