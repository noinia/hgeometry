module LowerEnvelope.NaiveSpec where

import           Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import           HGeometry.Combinatorial.Util
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Instances ()
import           HGeometry.LowerEnvelope
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Vector
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "lowerEnvelope tests" $ do
         prop "intersection on plane" $ \h1 h2 (h3 :: Plane R) ->
           verifyOnPlane h1 h2 h3
         it "asBoundedVertex" $
           let [h1,h2,h3] = inputs in
           asBoundedVertex (Three h1 h2 h3) `shouldBe`
             Just (BoundedVertex (Point3 10 10 10) (Set.fromList inputs))
         prop "belowall" $ \h1 h2 h3 ->
           case asBoundedVertex (Three h1 h2 (h3 :: Plane R)) of
             Nothing -> True
             Just v  -> v `belowAll` [h1,h2,h3]
         it "vertices inputs" $
           let [h1,h2,h3] = inputs
               p          = Point3 10 10 10
               v          = Vertex $ BoundedVertex p (Set.fromList inputs)
           in
             _boundedVertices (lowerEnvelope inputs)
           `shouldBe`
             (Map.fromList [(p, Set.fromList inputs)])
         it "halfEdges inputs" $
           let [h1,h2,h3] = inputs
               v          = Vertex $ BoundedVertex (Point3 10 10 10) (Set.fromList inputs)
           in
             _halfEdges (lowerEnvelope inputs)
           `shouldBe`
             [ HalfEdge v               UnboundedVertex h1
             , HalfEdge UnboundedVertex v               h2
             , HalfEdge v               UnboundedVertex h3
             , HalfEdge UnboundedVertex               v h1
             , HalfEdge v               UnboundedVertex h2
             , HalfEdge UnboundedVertex               v h3
             ]
             -- there still seems to be s.t. wrong with the order of the leftplanes


-- | verify that the intersection point indeed lis on all three planes
verifyOnPlane          :: (Fractional r, Ord r)
                       => Plane r -> Plane r -> Plane r -> Bool
verifyOnPlane h1 h2 h3 = case intersectionPoint h1 h2 h3 of
                           Nothing -> True
                           Just (Point3 x y _)  -> allEqual $
                                                   (evalAt $ Point2 x y) <$> Three h1 h2 h3
  where
    allEqual (Three a b c) = a == b && b == c


myEnv = lowerEnvelope inputs
-- myTriEnv = triangulatedLowerEnvelope inputs

inputs :: [Plane R]
inputs = [ Plane 1 0 0
         , Plane 0 1 0
         , Plane 0 0 10
         ]
