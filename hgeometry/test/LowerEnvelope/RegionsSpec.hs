module LowerEnvelope.RegionsSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import           HGeometry.Combinatorial.Util
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Instances ()
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope.Connected
import           HGeometry.Point
import           HGeometry.Vector
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

-- see also the test-with-ipe/test/VoronoiDiagram/VornoiSpec tests
--

-- TODO: I think fromVertexForm should also work for singleton inputs; i.e. just three planes intersecting in a vertex . That would be a good first testcase I think.
-- for the rest we may watn to use the VoronoiDiagram tests to generate the edges of the VD



spec :: Spec
spec = describe "lowerEnvelope tests" $ do
         prop "intersection on plane" $ \h1 h2 (h3 :: Plane R) ->
           verifyOnPlane h1 h2 h3
         it "asVertex" $ do
           computeVertexForm inputs `shouldBe`
             Map.singleton (Point3 10 10 10) (fromCCWList inputs)
         prop "belowall" $ \h1 h2 (h3 :: Plane R) ->
           let vertices = Map.toAscList $ computeVertexForm [h1,h2,h3]
           in all (\(v, _) -> v `belowAll` [h1,h2,h3]) vertices

         it "singleton diagram" $ do
           let v = Point2 10 10 :: Point 2 R
           [h1,h2,h3] <- pure inputs
           (asMap $ bruteForceLowerEnvelope inputs) `shouldBe`
             mkNEMap
               [ (h1, Unbounded (Vector2 1 1)    (NonEmpty.singleton v) (Vector2 0 1))
               , (h2, Unbounded (Vector2 (-1) 0) (NonEmpty.singleton v) (Vector2 (-1) (-1)))
               , (h3, Unbounded (Vector2 0 (-1)) (NonEmpty.singleton v) (Vector2 1 0))
               ]

mkNEMap = NEMap.fromList . NonEmpty.fromList

-- | verify that the intersection point indeed lis on all three planes
verifyOnPlane          :: (Fractional r, Ord r)
                       => Plane r -> Plane r -> Plane r -> Bool
verifyOnPlane h1 h2 h3 = case intersectionPoint (Three h1 h2 h3) of
                           Nothing -> True
                           Just (Point3 x y _)  -> allEqual $
                                                   evalAt (Point2 x y) <$> Three h1 h2 h3
  where
    allEqual (Three a b c) = a == b && b == c

-- | copied from the module
belowAll   :: (Plane_ plane r, Ord r, Num r, Foldable f) => Point 3 r -> f plane -> Bool
belowAll v = all (\h -> verticalSideTest v h /= GT)

myEnv = bruteForceLowerEnvelope inputs
-- myTriEnv = triangulatedLowerEnvelope inputs

inputs :: [Plane R]
inputs = [ Plane 1 0 0
         , Plane 0 1 0
         , Plane 0 0 10
         ]
