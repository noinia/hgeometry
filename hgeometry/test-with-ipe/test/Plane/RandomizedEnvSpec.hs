{-# LANGUAGE QuasiQuotes #-}
module Plane.RandomizedEnvSpec
  where

import Control.Lens hiding (Prism)
import System.OsPath
import Ipe
import System.Random
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
import Plane.Randomized2
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import HGeometry.Ext

--------------------------------------------------------------------------------

instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> arbitrary

spec :: Spec
spec = describe "Plane.RandomizedEnvSpec" $ do
         modifyMaxSize (const 60) $ do
           prop "new brute force same as original" $
             \(planes :: NESet.NESet MyPlane) ->
               verticesOf (bruteForceVertices (Sample (toList planes)
                                                      (length planes) [] (length planes)))
               ===
               Map.keys (Original.computeVertexForm planes)

           prop "dummy" $
             \(planes :: NESet.NESet MyPlane) ->
               let input = Sample (toList planes) (length planes) [] (length planes)
               in case bruteForceTriangulatedEnvelope input of
                    (_, Nothing)      -> discard
                    (env, Just _bBox) -> show env === "foo"

           prop "randomized2 same as (new) brute force" $
             \(planes :: NESet.NESet MyPlane) (gen :: StdGen) ->
               let input = Sample (toList planes) (length planes) [] (length planes) in
               verticesOf (randomizedVertices gen input)
               ===
               verticesOf (bruteForceVertices input)

         runIO test


verticesOf :: (Plane_ plane r, Ord r, Fractional r)
           => MonoidalMap (Vertex r plane) conflictLists -> [Point 3 r]
verticesOf = sort . map location   . MonoidalMap.keys




--------------------------------------------------------------------------------

test = writeIpeFile [osp|env.ipe|] $ singlePageFromContent $ draw env
  where
    input   = Sample (toList planes) (length planes) [] (length planes)
    (env,_) = bruteForceTriangulatedEnvelope input

    planes = NonEmpty.fromList
             [ Plane (-1) 3 1
             , Plane 1.66666 1.66666 (-3)
             , Plane 2.66666 (-1) 0.5
             , Plane 0 0 1
             ]


draw :: (Plane_ plane r, Ord plane, Ord r, Fractional r)
     => TriangulatedLowerEnvelope r plane -> [IpeObject r]
draw = ifoldMap draw'
  where
    draw' h = foldMap draw''
      where
        draw'' (cell :+ cl) = case cell of
          Triangular u v w -> [iO $ defIO $ Triangle (location2 u) (location2 v) (location2 w)
                              ]
          Cone v           -> [iO $ defIO $ location2 v]
          ClippedCone u v  -> [iO $ defIO $ ClosedLineSegment (location2 u) (location2 v)]
