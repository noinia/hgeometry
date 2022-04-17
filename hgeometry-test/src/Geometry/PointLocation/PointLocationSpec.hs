module Geometry.PointLocation.PointLocationSpec where

import Control.Lens
import Data.Ext
import Geometry.PointLocation.PersistentSweep
import Data.RealNumber.Rational
import Geometry.Point
import Geometry.Polygon
import Ipe
import Test.Hspec

--------------------------------------------------------------------------------

type R = RealNumber 5


spec :: Spec
spec = describe "PointLoction Tests" $ do
         manual "src/Geometry/PointLocation/manual.ipe"

manual    :: FilePath -> Spec
manual fp = runIO (readSinglePageFileThrow fp) >>= spec'

spec'      :: IpePage R -> Spec
spec' page = let points = readAll @(Point 2 R)          page
                 pgs'   = readAll @(SimplePolygon () R) page
                 pgs    = map (over core inPolygonDS) pgs'
             in sequence_ [ query q pg | q <- points, pg <- pgs ]

query                           :: Point 2 R        :+ IpeAttributes IpeSymbol R
                                -> InPolygonDS () R :+ IpeAttributes Path R
                                -> Spec
query (q :+ qAts) (pg :+ pgAts) = it ("querying " <> show q <> " in pg: " <> show (color pgAts)) $
                                    let res = pointInPolygon q pg
                                    in if color qAts == color pgAts
                                       then res `shouldBe` In
                                       else res `shouldBe` Out
  where
    color ats = ats^?_Attr SFill


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
