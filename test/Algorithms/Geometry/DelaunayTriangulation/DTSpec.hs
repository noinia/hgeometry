module Algorithms.Geometry.DelaunayTriangulation.DTSpec where

import Test.Hspec
import Control.Lens hiding (only)
import Data.Geometry
import Algorithms.Geometry.DelaunayTriangulation.Types
import qualified Algorithms.Geometry.DelaunayTriangulation.DivideAndConqueror as DC
import qualified Algorithms.Geometry.DelaunayTriangulation.Naive              as Naive
import qualified Data.List.NonEmpty as NonEmpty
import Data.Geometry.Ipe
import Data.Ext
import Data.Traversable(traverse)
import qualified Data.CircularList as C
import qualified Data.Map as M
import qualified Data.Vector as V


dtEdges = edges . DC.delaunayTriangulation

take' i = NonEmpty.fromList . NonEmpty.take i


isShiftOf         :: Eq a => C.CList a -> C.CList a -> Bool
xs `isShiftOf` ys = let rest = tail . C.leftElements
                    in maybe False (\xs' -> rest xs' == rest ys) $
                         C.focus ys >>= flip C.rotateTo xs

sameAsNaive       :: (Fractional r, Ord r, Show p, Show r)
                  => String -> NonEmpty.NonEmpty (Point 2 r :+ p) -> Spec
sameAsNaive s pts = it ("Divide And Conqueror same answer as Naive on " ++ s) $
                      (Naive.delaunayTriangulation pts
                       `sameEdges`
                       DC.delaunayTriangulation pts) `shouldBe` True

sameEdges             :: Triangulation p r -> Triangulation p r -> Bool
triA `sameEdges` triB = all (\(a,b) -> (adjA V.! a) `isShiftOf` (adjB V.! b))
                      $ zip (M.elems $ triA^.vertexIds) (M.elems $ triB^.vertexIds)
  where
    adjA = triA^.neighbours
    adjB = triB^.neighbours

spec :: Spec
spec = do
  describe "Testing Divide and Conqueror Algorithm for Delaunay Triangulation" $ do
    it "singleton " $ do
      dtEdges (take' 1 myPoints) `shouldBe` []
    sameAsNaive "myPoints" myPoints

-- main = do
--          page <- readSinglePageFile "/Users/frank/tmp/dt.ipe"
--          let pts = page^..content.traverse._IpeUse
--              dt  = delaunayTriangulation $ NonEmpty.fromList pts
--          print dt






myPoints :: NonEmpty.NonEmpty (Point 2 Rational :+ ())
myPoints = NonEmpty.fromList . map only $
           [ point2 1  3
           , point2 4  26
           , point2 5  17
           , point2 6  7
           -- , point2 12 16
           -- , point2 19 4
           -- , point2 20 0
           -- , point2 20 11
           -- , point2 23 23
           -- , point2 31 14
           -- , point2 33 5
           ]
