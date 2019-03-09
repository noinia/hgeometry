module Algorithms.Geometry.LinearProgramming.LinearProgrammingRICSpec where

import Algorithms.Geometry.LinearProgramming.LP2DRIC
import Control.Lens
import Data.Geometry
import Data.Geometry.HalfSpace
import Data.Geometry.Properties
import Data.Proxy
import Data.Ratio
import Test.Hspec
-- import Util

--------------------------------------------------------------------------------

testLP :: LinearProgram 2 Rational
testLP = LinearProgram (Vector2 0 (-1)) [ above $ Line (Point2 0 0) (Vector2 2    1)
                                        , above $ Line (Point2 0 3) (Vector2 1    (-1))
                                        , above $ Line (Point2 0 1) (Vector2 3    1)
                                        , above $ Line (Point2 0 5) (Vector2 2    (-1))
                                        ]


test :: IO (Maybe (Point 2 Rational))
test = solveBoundedLinearProgram testLP

--------------------------------------------------------------------------------

inAllHalfpaces       :: (Arity d, Ord r, Num r)
                     => Maybe (Point d r) -> LinearProgram d r -> Bool
inAllHalfpaces mp lp = case mp of
    Nothing -> True
    Just p  -> all (p `intersects`) $ lp^.constraints

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "2D LP Tests" $ do
         it "in all hafspaces" $
           inAllHalfpaces (solveBoundedLinearProgram' testLP) testLP
           `shouldBe` True
         -- prop "in all hafspaces" $
         --   \(lp :: LinearProgram 2 Rational) ->
         --   inAllHalfpaces (solveBoundedLinearProgram' lp) lp
