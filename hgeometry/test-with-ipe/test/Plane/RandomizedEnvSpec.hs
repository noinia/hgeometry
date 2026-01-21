module Plane.RandomizedEnvSpec
  where

import Data.List (sort)
import Data.Foldable
import HGeometry.Kernel
import Plane.BruteForce
import Plane.Sample
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.WithTempFile
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import HGeometry.Plane.LowerEnvelope.Connected.BruteForce qualified as Original
import R
import Plane.LowerEnvelopeSpec (MyPlane(..))
import Data.Set.NonEmpty qualified as NESet
import Data.Map qualified as Map
import Data.Map.Monoidal qualified as MonoidalMap
import Data.Map.Monoidal (MonoidalMap)

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Plane.RandomizedEnvSpec" $ do
         modifyMaxSize (const 60) $
           prop "new brute force same as original" $
             \(planes :: NESet.NESet MyPlane) ->
               verticesOf (bruteForceVertices (Sample (toList planes)
                                                      (length planes) [] (length planes)))
               ===
               Map.keys (Original.computeVertexForm planes)



verticesOf :: (Plane_ plane r, Ord r, Fractional r)
           => MonoidalMap (Vertex plane) conflictLists -> [Point 3 r]
verticesOf = sort . map location   . MonoidalMap.keys
