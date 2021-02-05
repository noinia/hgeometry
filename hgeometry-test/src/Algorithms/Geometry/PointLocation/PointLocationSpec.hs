module Algorithms.Geometry.PointLocation.PointLocationSpec where

import Algorithms.Geometry.PointLocation.PersistentSweep as Sweep
import Data.RealNumber.Rational

type R = RealNumber 5


spec :: Spec
spec = pure ()



-- -- | Point sets per color, Crosses form the solution
-- readInput    :: FilePath -> IO (Either ConversionError [TestCase R])
-- readInput fp = fmap f <$> readSinglePageFile fp
--   where
--     f page = [ TestCase [p^.core.symbolPoint :+ () | p <- pSet] (solutionOf pSet)
--              | pSet <- byStrokeColour syms
--              ]
--       where
--         syms = page^..content.traverse._IpeUse

--         -- | Crosses form a solution
--         isInSolution s = s^.core.symbolName == "mark/cross(sx)"

--         right = either (const Nothing) Just
--         solutionOf = right . twoOrThreeFromList . map (^.core.symbolPoint) . filter isInSolution
